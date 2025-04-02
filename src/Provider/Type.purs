module Cardano.Provider.Type
  ( AffE
  , Provider
  ) where

import Cardano.Provider.Error (ClientError, GetTxMetadataError)
import Cardano.Types
  ( Address
  , AuxiliaryData
  , DataHash
  , NetworkId
  , PlutusData
  , PoolPubKeyHash
  , ScriptHash
  , ScriptRef
  , StakePubKeyHash
  , Transaction
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  , UtxoMap
  )
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Chain as Chain
import Cardano.Types.DelegationsAndRewards (DelegationsAndRewards)
import Cardano.Types.EraSummaries (EraSummaries)
import Cardano.Provider.OgmiosTypes (OgmiosTxOutRef, TxEvaluationR, OgmiosTxOut)
import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

type AffE (a :: Type) = Aff (Either ClientError a)

type Provider =
  { getDatumByHash :: DataHash -> AffE (Maybe PlutusData)
  , getScriptByHash :: ScriptHash -> AffE (Maybe ScriptRef)
  , getTxAuxiliaryData ::
      TransactionHash
      -> Aff (Either GetTxMetadataError AuxiliaryData)
  , getUtxoByOref :: TransactionInput -> AffE (Maybe TransactionOutput)
  , getOutputAddressesByTxHash :: TransactionHash -> AffE (Array Address)
  , doesTxExist :: TransactionHash -> AffE Boolean
  , utxosAt :: Address -> AffE UtxoMap
  , getChainTip :: AffE Chain.Tip
  , getCurrentEpoch :: Aff BigNum
  -- TODO Capture errors from all backends
  , submitTx :: Transaction -> AffE TransactionHash
  , evaluateTx :: Transaction -> Map OgmiosTxOutRef OgmiosTxOut -> Aff TxEvaluationR
  , getEraSummaries :: AffE EraSummaries
  , getPoolIds :: AffE (Array PoolPubKeyHash)
  , getPubKeyHashDelegationsAndRewards ::
      NetworkId -> StakePubKeyHash -> AffE (Maybe DelegationsAndRewards)
  , getValidatorHashDelegationsAndRewards ::
      NetworkId -> ScriptHash -> AffE (Maybe DelegationsAndRewards)
  }

