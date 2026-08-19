module Cardano.Provider.Types.Governance.Proposal
  ( Proposal
  ) where

import Cardano.Provider.Types.Governance.ProposalType (ProposalType)
import Cardano.Types (BigNum, RewardAddress)

type Proposal =
  { proposalType :: ProposalType
  , deposit :: BigNum
  , returnAddress :: RewardAddress
  }
