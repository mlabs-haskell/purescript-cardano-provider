module Cardano.Provider.Types.Governance.DrepInfo
  ( DrepInfo
  ) where

import Cardano.Types (Coin)

type DrepInfo =
  { deposit :: Coin
  , votingPower :: Coin
  }
