module Cardano.Provider.Types.Governance.DrepInfo
  ( DrepInfo
  ) where

import Cardano.Types (BigNum)

type DrepInfo =
  { deposit :: BigNum
  , votingPower :: BigNum
  }
