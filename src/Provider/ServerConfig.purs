module Cardano.Provider.ServerConfig
  ( Host
  , ServerConfig
  , mkHttpUrl
  , mkServerUrl
  , mkWsUrl
  ) where

import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid ((<>))
import Data.String (Pattern(..), null, stripPrefix, stripSuffix)
import Data.UInt (UInt)
import Data.UInt as UInt

type Host = String

type ServerConfig =
  { port :: UInt
  , host :: Host
  , secure :: Boolean
  , path :: Maybe String
  }

mkHttpUrl :: ServerConfig -> String
mkHttpUrl = mkServerUrl "http"

mkWsUrl :: ServerConfig -> String
mkWsUrl = mkServerUrl "ws"

mkServerUrl :: String -> ServerConfig -> String
mkServerUrl protocol cfg =
  (if cfg.secure then (protocol <> "s") else protocol)
    <> "://"
    <> cfg.host
    <> ":"
    <> UInt.toString cfg.port
    <</>> fromMaybe "" cfg.path

-- | Concat two strings with "/" in the middle, but stripping multiple slashes.
-- No slash if second string empty.
concatPaths :: String -> String -> String
concatPaths a b =
  if null right then left
  else left <> "/" <> right

  where
  left = fromMaybe a (stripSuffix (Pattern "/") a)
  right = fromMaybe b (stripPrefix (Pattern "/") b)

infixr 5 concatPaths as <</>> -- </> is taken