module Cardano.Provider.Types.Governance.Proposal
  ( Proposal
  ) where

import Cardano.Provider.Types.Governance.ProposalType (ProposalType)
import Cardano.Types (Coin, RewardAddress)

type Proposal =
  { proposalType :: ProposalType
  , deposit :: Coin
  , returnAddress :: RewardAddress
  }
