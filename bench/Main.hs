module Main (main) where

import Data.Text (Text)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (withApplication)
import Prometheus.Servant (defaultMetrics, prometheusMiddleware)
import Servant
  ( Capture
  , Get
  , JSON
  , Proxy (..)
  , serve
  , (:>)
  )
import System.Process (callCommand)

type BenchApi = "hello" :> Capture "name" Text :> Get '[JSON] Text

benchApi :: Proxy BenchApi
benchApi = Proxy

server :: Application
server = serve benchApi pure

benchApp :: Application -> IO ()
benchApp app = do
  withApplication (pure app) $ \port ->
    callCommand $
      "wrk -c 30 -d 20s --latency -s bench/wrk.lua -t 2 'http://localhost:"
        ++ show port
        ++ "'"

main :: IO ()
main = do
  benchApp $ prometheusMiddleware defaultMetrics benchApi server
  benchApp server
