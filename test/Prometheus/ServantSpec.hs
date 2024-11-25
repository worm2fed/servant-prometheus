module Prometheus.ServantSpec (spec) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (ok200)
import Network.Wai (Application)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (Port, withApplication)
import Prometheus qualified as P
import Servant
  ( Capture
  , CaptureAll
  , Delete
  , Get
  , JSON
  , NoContent (..)
  , Post
  , Proxy (..)
  , QueryParam
  , Raw
  , ReqBody
  , Server
  , serve
  , (:<|>) (..)
  , (:>)
  )
import Servant qualified
import Servant.Client
  ( BaseUrl (..)
  , ClientError
  , ClientM
  , Scheme (..)
  , client
  , mkClientEnv
  , runClientM
  )
import Servant.Test.ComprehensiveAPI (comprehensiveAPI)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

import Prometheus.Servant (Metrics (..), defaultMetrics, prometheusMiddleware)
import Prometheus.Servant.Internal (Endpoint (..), HasEndpoint (..))

-- * Spec

spec :: Spec
spec = describe "servant-prometheus" $ do
  let getEp :<|> postEp :<|> deleteEp :<|> proxyEp = client testApi

  it "collects number of request" $
    withApp $ \port -> do
      mgr <- newManager defaultManagerSettings
      let runFn :: ClientM a -> IO (Either ClientError a)
          runFn fn = runClientM fn (mkClientEnv mgr (BaseUrl Http "localhost" port ""))
      _ <- runFn $ getEp "name" Nothing
      _ <- runFn $ postEp (Greet "hi")
      _ <- runFn $ deleteEp "blah"
      _ <- runFn $ proxyEp ["some", "proxy", "route"] methodGet

      let Metrics{..} = defaultMetrics
      latencies <- P.getVectorWith mLatency P.getHistogram
      map fst latencies
        `shouldBe` [ ("/greet", "POST", "200")
                   , ("/greet/:greetid", "DELETE", "200")
                   , ("/hello/:name", "GET", "200")
                   , ("/proxy/*", "RAW", "200")
                   ]
      map (sum . map snd . Map.toList . snd) latencies
        `shouldBe` [1, 1, 1, 1]

  it "is comprehensive" $ do
    let !_typeLevelTest = prometheusMiddleware defaultMetrics comprehensiveAPI
    True `shouldBe` True

  it "enumerates the parts of an API correctly" $
    enumerateEndpoints testApi
      `shouldBe` [ Endpoint ["hello", ":name"] "GET"
                 , Endpoint ["greet"] "POST"
                 , Endpoint ["greet", ":greetid"] "DELETE"
                 , Endpoint ["proxy", "*"] "RAW"
                 ]

-- * Example

-- | A greet message data type
newtype Greet = Greet {_msg :: Text}
  deriving stock (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- | API specification
type TestApi =
  -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
  "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet
    -- POST /greet with a Greet as JSON in the request body,
    --             returns a Greet as JSON
    :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet
    -- DELETE /greet/:greetid
    :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent
    -- GET /proxy/some/proxy/route
    :<|> "proxy" :> CaptureAll "proxyRoute" Text :> Raw

testApi :: Proxy TestApi
testApi = Proxy

-- | Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH :<|> proxyH
  where
    helloH name Nothing = helloH name (Just False)
    helloH name (Just False) = pure . Greet $ "Hello, " <> name
    helloH name (Just True) = pure . Greet . T.toUpper $ "Hello, " <> name

    postGreetH = pure

    deleteGreetH _ = pure NoContent

    proxyH :: [Text] -> Servant.Tagged Servant.Handler Wai.Application
    proxyH _ = Servant.Tagged $ \_ responder -> responder $ Wai.responseLBS ok200 [] "success"

-- | Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

withApp :: (Port -> IO a) -> IO a
withApp =
  withApplication (pure $ prometheusMiddleware defaultMetrics testApi test)
