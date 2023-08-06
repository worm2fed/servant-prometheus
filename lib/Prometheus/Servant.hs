module Prometheus.Servant
  ( prometheusMiddleware
  , Metrics (..)
  , defaultMetrics
  , RequestLatencyMetric
  , ActiveRequestsMetric
  ) where

import Control.Exception (finally)
import Data.Data (Proxy)
import Data.Ratio ((%))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Types (Status (..))
import Network.Wai (Middleware, responseStatus)
import Prometheus qualified as P
import System.Clock
  ( Clock (Monotonic)
  , diffTimeSpec
  , getTime
  , s2ns
  , toNanoSecs
  )

import Prometheus.Servant.Internal (Endpoint (..), HasEndpoint (..))

-- | 'Middleware' to observe 'Metrics'.
prometheusMiddleware
  :: (P.Label mLatencyLabel, P.Label mActiveLabel, HasEndpoint api)
  => Metrics mLatencyLabel mActiveLabel
  -> Proxy api
  -> Middleware
prometheusMiddleware Metrics{..} proxy application request sendResponse = do
  case getEndpoint proxy request of
    Just endpoint -> do
      let mActiveLabel = mGetActiveLabels endpoint
      !start <- getTime Monotonic
      P.withLabel mActive mActiveLabel P.incGauge
      application request $ \response -> do
        let mLatencyLabel = mGetLatencyLabels endpoint (responseStatus response)
        sendResponse response `finally` do
          !end <- getTime Monotonic
          let latency = fromRational $ toNanoSecs (end `diffTimeSpec` start) % s2ns
          P.withLabel mLatency mLatencyLabel $ flip P.observe latency
          P.withLabel mActive mActiveLabel P.decGauge
    Nothing -> application request sendResponse

-- | Supported metrics and a function to get relevant labels from 'Endpoint'.
data Metrics mLatencyLabel mActiveLabel = Metrics
  { mLatency :: RequestLatencyMetric mLatencyLabel
  , mGetLatencyLabels :: Endpoint -> Status -> mLatencyLabel
  , mActive :: ActiveRequestsMetric mActiveLabel
  , mGetActiveLabels :: Endpoint -> mActiveLabel
  }

-- | Default 'Metrics'.
defaultMetrics :: Metrics P.Label3 P.Label2
defaultMetrics =
  Metrics
    { mLatency = mHttpRequestLatency
    , mGetLatencyLabels = getHttpRequestLatencyLabels
    , mActive = mHttpActiveRequests
    , mGetActiveLabels = getHttpActiveRequestsLabels
    }

-- | Request latency metric parametrized with some label @l@.
type RequestLatencyMetric l = P.Vector l P.Histogram

-- | Metric to measure HTTP server request latency.
mHttpRequestLatency :: RequestLatencyMetric P.Label3
mHttpRequestLatency =
  P.unsafeRegister
    . P.vector ("route_name", "method", "status_code")
    $ P.histogram i P.defaultBuckets
  where
    i =
      P.Info
        "http_request_duration_seconds"
        "The HTTP server request latencies in seconds."
{-# NOINLINE mHttpRequestLatency #-}

-- | Defines how to get labels for 'mHttpRequestLatency' from 'Endpoint'.
getHttpRequestLatencyLabels :: Endpoint -> Status -> P.Label3
getHttpRequestLatencyLabels Endpoint{..} status =
  ( "/" <> T.intercalate "/" ePathSegments
  , T.decodeUtf8 eMethod
  , T.pack . show $ statusCode status
  )

-- | Active requests metric parametrized with some label @l@.
type ActiveRequestsMetric l = P.Vector l P.Gauge

-- | Metric to track HTTP active requests.
mHttpActiveRequests :: ActiveRequestsMetric P.Label2
mHttpActiveRequests =
  P.unsafeRegister
    . P.vector ("route_name", "method")
    $ P.gauge i
  where
    i =
      P.Info
        "http_active_requests"
        "The HTTP active requests."
{-# NOINLINE mHttpActiveRequests #-}

-- | Defines how to get labels for 'mHttpActiveRequests' from 'Endpoint'.
getHttpActiveRequestsLabels :: Endpoint -> P.Label2
getHttpActiveRequestsLabels Endpoint{..} =
  ( "/" <> T.intercalate "/" ePathSegments
  , T.decodeUtf8 eMethod
  )
