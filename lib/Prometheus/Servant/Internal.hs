module Prometheus.Servant.Internal
  ( Endpoint (..)
  , HasEndpoint (..)
  ) where

import Control.Monad (MonadPlus (..))
import Data.Hashable (Hashable (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHC.Types (Type)
import Network.HTTP.Types (Method)
import Network.Wai (Request (..))
import Servant.API

data Endpoint = Endpoint
  { ePathSegments :: [Text]
  , eMethod :: Method
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

class HasEndpoint a where
  getEndpoint :: Proxy a -> Request -> Maybe Endpoint
  enumerateEndpoints :: Proxy a -> [Endpoint]

instance HasEndpoint EmptyAPI where
  getEndpoint _ _ = Nothing
  enumerateEndpoints _ = []

instance (HasEndpoint (a :: Type), HasEndpoint (b :: Type)) => HasEndpoint (a :<|> b) where
  getEndpoint _ req =
    getEndpoint (Proxy :: Proxy a) req
      `mplus` getEndpoint (Proxy :: Proxy b) req

  enumerateEndpoints _ =
    enumerateEndpoints (Proxy :: Proxy a)
      <> enumerateEndpoints (Proxy :: Proxy b)

instance
  (KnownSymbol (path :: Symbol), HasEndpoint (sub :: Type))
  => HasEndpoint (path :> sub)
  where
  getEndpoint _ req =
    case pathInfo req of
      p : ps | p == T.pack (symbolVal (Proxy :: Proxy path)) -> do
        Endpoint{..} <- getEndpoint (Proxy :: Proxy sub) req{pathInfo = ps}
        pure $ Endpoint (p : ePathSegments) eMethod
      _ -> Nothing

  enumerateEndpoints _ =
    let endpoints = enumerateEndpoints (Proxy :: Proxy sub)
        currentSegment = T.pack $ symbolVal (Proxy :: Proxy path)
        qualify Endpoint{..} = Endpoint (currentSegment : ePathSegments) eMethod
     in map qualify endpoints

instance
  (KnownSymbol (capture :: Symbol), HasEndpoint (sub :: Type))
  => HasEndpoint (Capture' mods capture a :> sub)
  where
  getEndpoint _ req =
    case pathInfo req of
      _ : ps -> do
        Endpoint{..} <- getEndpoint (Proxy :: Proxy sub) req{pathInfo = ps}
        let p = T.pack $ (':' :) $ symbolVal (Proxy :: Proxy capture)
        pure $ Endpoint (p : ePathSegments) eMethod
      _ -> Nothing
  enumerateEndpoints _ =
    let endpoints = enumerateEndpoints (Proxy :: Proxy sub)
        currentSegment = T.pack $ (':' :) $ symbolVal (Proxy :: Proxy capture)
        qualify Endpoint{..} = Endpoint (currentSegment : ePathSegments) eMethod
     in map qualify endpoints

instance HasEndpoint (sub :: Type) => HasEndpoint (Summary d :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (Description d :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (Header' mods h a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (Fragment a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance
  HasEndpoint (sub :: Type)
  => HasEndpoint (QueryParam' mods (h :: Symbol) a :> sub)
  where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (QueryParams (h :: Symbol) a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (QueryFlag h :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (ReqBody' mods cts a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (StreamBody' mods framing cts a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (RemoteHost :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (IsSecure :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (HttpVersion :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (Vault :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (WithNamedContext x y sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoint (Verb method status cts a) where
  getEndpoint _ req = case pathInfo req of
    [] | requestMethod req == method -> Just (Endpoint [] method)
    _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

  enumerateEndpoints _ = [Endpoint mempty method]
    where
      method = reflectMethod (Proxy :: Proxy method)

instance ReflectMethod method => HasEndpoint (NoContentVerb method) where
  getEndpoint _ req = case pathInfo req of
    [] | requestMethod req == method -> Just (Endpoint [] method)
    _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

  enumerateEndpoints _ = [Endpoint mempty method]
    where
      method = reflectMethod (Proxy :: Proxy method)

instance ReflectMethod method => HasEndpoint (Stream method status framing ct a) where
  getEndpoint _ req = case pathInfo req of
    [] | requestMethod req == method -> Just (Endpoint [] method)
    _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

  enumerateEndpoints _ = [Endpoint mempty method]
    where
      method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoint Raw where
  getEndpoint _ _ = Just (Endpoint [] "RAW")
  enumerateEndpoints _ = [Endpoint [] "RAW"]

instance HasEndpoint (sub :: Type) => HasEndpoint (CaptureAll (h :: Symbol) a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: Type) => HasEndpoint (BasicAuth (realm :: Symbol) a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)
