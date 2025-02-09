module Cardano.Provider.ServerConfig
  ( Host
  , ServerConfig
  ) where

import Data.Maybe (Maybe)
import Data.UInt (UInt)

type Host = String

type ServerConfig =
  { port :: UInt
  , host :: Host
  , secure :: Boolean
  , path :: Maybe String
  }

