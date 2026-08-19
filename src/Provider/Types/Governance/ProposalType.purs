module Cardano.Provider.Types.Governance.ProposalType
  ( ProposalType
      ( ChangeProtocolParameters
      , TriggerHardFork
      , TreasuryWithdrawal
      , NoConfidence
      , NewCommittee
      , NewConstitution
      , Info
      )
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data ProposalType
  = ChangeProtocolParameters
  | TriggerHardFork
  | TreasuryWithdrawal
  | NoConfidence
  | NewCommittee
  | NewConstitution
  | Info

derive instance Generic ProposalType _
derive instance Eq ProposalType

instance Show ProposalType where
  show = genericShow
