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
  , getGovActionType
  ) where

import Prelude

import Cardano.Types
  ( GovernanceAction
      ( ChangePParams
      , TriggerHF
      , TreasuryWdrl
      , NoConfidence
      , NewCommittee
      , NewConstitution
      , Info
      )
  ) as GovAction
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

getGovActionType :: GovAction.GovernanceAction -> ProposalType
getGovActionType =
  case _ of
    GovAction.ChangePParams _ -> ChangeProtocolParameters
    GovAction.TriggerHF _ -> TriggerHardFork
    GovAction.TreasuryWdrl _ -> TreasuryWithdrawal
    GovAction.NoConfidence _ -> NoConfidence
    GovAction.NewCommittee _ -> NewCommittee
    GovAction.NewConstitution _ -> NewConstitution
    GovAction.Info -> Info
