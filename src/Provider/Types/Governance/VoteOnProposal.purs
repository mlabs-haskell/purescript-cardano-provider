module Cardano.Provider.Types.Governance.VoteOnProposal
  ( VoteOnProposal
  ) where

import Cardano.Types (Vote, Voter)

type VoteOnProposal =
  { voter :: Voter
  , vote :: Vote
  }
