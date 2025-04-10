module Cardano.Provider.OgmiosTypes
  ( class DecodeOgmios
  , ExecutionUnits
  , JsonRpc2Response
  , OgmiosAddress
  , OgmiosDatum
  , OgmiosDecodeError(ErrorResponse, InvalidRpcError, InvalidRpcResponse)
  , OgmiosError(OgmiosError)
  , OgmiosRedeemerPtr
  , OgmiosScript
  , OgmiosTxId
  , OgmiosTxIn
  , OgmiosTxOut
  , OgmiosTxOutRef
  , RedeemerPointer
  , ScriptFailure
      ( ExtraRedeemers
      , MissingRequiredDatums
      , MissingRequiredScripts
      , ValidatorFailed
      , UnknownInputReferencedByRedeemer
      , NonScriptInputReferencedByRedeemer
      , NoCostModelForLanguage
      , InternalLedgerTypeConversionError
      , IllFormedExecutionBudget
      )
  , TxEvaluationFailure(UnparsedError, AdditionalUtxoOverlap, ScriptFailures)
  , TxEvaluationR(TxEvaluationR)
  , TxEvaluationResult(TxEvaluationResult)
  , decodeAesonJsonRpc2Response
  , decodeErrorOrResult
  , decodeOgmios
  , decodeRedeemerPointer
  , decodeResult
  , makeDecodeOgmios
  , ogmiosDecodeErrorToError
  , pprintOgmiosDecodeError
  , pprintOgmiosError
  , redeemerTagFromString
  , redeemerTypeMismatch
  , showRedeemerPointer
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch, AtKey, MissingValue)
  , caseAesonArray
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  , getFieldOptional
  , getFieldOptional'
  , printJsonDecodeError
  , stringifyAeson
  )
import Cardano.Types (Bech32String, BigNum, RedeemerTag, ScriptHash, ScriptRef, Value)
import Cardano.Types (RedeemerTag(Spend, Mint, Cert, Reward, Vote, Propose)) as RedeemerTag
import Data.Array (fromFoldable) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(Cons), singleton) as List
import Data.List (List)
import Data.Map (Map)
import Data.Map (alter, empty, fromFoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.These (These(That, Both), theseLeft, theseRight)
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect.Exception (Error, error)

type OgmiosTxIn =
  { txId :: OgmiosTxId
  , index :: Prim.Int
  }

type OgmiosTxOutRef =
  { txId :: String
  , index :: UInt
  }

type OgmiosTxOut =
  { address :: OgmiosAddress
  , value :: Value
  , datumHash :: Maybe String
  , datum :: Maybe String
  , script :: Maybe ScriptRef
  }

type OgmiosAddress = Bech32String

type RedeemerPointer = { redeemerTag :: RedeemerTag, redeemerIndex :: UInt }

type ExecutionUnits = { memory :: BigNum, steps :: BigNum }

showRedeemerPointer :: RedeemerPointer -> String
showRedeemerPointer ptr = show ptr.redeemerTag <> ":" <> show ptr.redeemerIndex

type OgmiosDatum = String
type OgmiosScript = String
type OgmiosTxId = String

-- DecodeOgmios ----------------------------------------------------------------

-- | Variation of DecodeAeson for ogmios response, defines how to parse full ogmios reponse.
-- We usually parse just the content of the "result" field,
-- but sometimes also "error" field, hence a class other than DecodeAeson.
class DecodeOgmios o where
  decodeOgmios :: Aeson -> Either OgmiosDecodeError o

-- | Given how to parse result or error fields,
-- defines a parser of the full json2rpc response.
makeDecodeOgmios
  :: forall o
   . These
       { parseError :: Aeson -> Either JsonDecodeError o }
       { parseResult :: Aeson -> Either JsonDecodeError o }
  -> Aeson
  -> Either OgmiosDecodeError o
makeDecodeOgmios decoders aeson = do
  json <- lmap InvalidRpcResponse $ decodeAesonJsonRpc2Response aeson
  let merr = _.parseError <$> theseLeft decoders <*> json.error
  let mres = _.parseResult <$> theseRight decoders <*> json.result
  case (mres /\ merr) of
    -- Expected result, got it
    Just (Right x) /\ _ -> pure x
    -- Expected result, got it in a wrong format
    Just (Left err) /\ _ -> Left $ InvalidRpcResponse err
    -- Got an expected error
    _ /\ Just (Right x) -> pure x
    -- Got an unexpected error
    _ -> do
      err :: Maybe OgmiosError <- sequence $
        lmap InvalidRpcError <<< decodeAeson <$> json.error
      Left $ ErrorResponse err

-- | Decode "result" field of ogmios response.
decodeResult
  :: forall o
   . (Aeson -> Either JsonDecodeError o)
  -> Aeson
  -> Either OgmiosDecodeError o
decodeResult decodeAeson = makeDecodeOgmios $ That { parseResult: decodeAeson }

-- | Decode "result" field or if absent the error field of ogmios response.
decodeErrorOrResult
  :: forall o
   . { parseError :: (Aeson -> Either JsonDecodeError o) }
  -> { parseResult :: (Aeson -> Either JsonDecodeError o) }
  -> Aeson
  -> Either OgmiosDecodeError o
decodeErrorOrResult err res = makeDecodeOgmios $ Both err res

-- | Structure of all json rpc websocket responses
-- described in: https://ogmios.dev/getting-started/basics/
type JsonRpc2Response =
  { jsonrpc :: String
  -- methodname is not always present if `error` is not empty
  , method :: Maybe String
  , result :: Maybe Aeson
  , error :: Maybe Aeson
  , id :: Maybe String
  }

decodeAesonJsonRpc2Response :: Aeson -> Either JsonDecodeError JsonRpc2Response
decodeAesonJsonRpc2Response =
  caseAesonObject (Left (TypeMismatch "Object")) \o -> do
    jsonrpc <- getField o "jsonrpc"
    method <- getFieldOptional o "method"
    result <- getFieldOptional o "result"
    error <- getFieldOptional o "error"
    id <- getFieldOptional' o "id"
    pure
      { jsonrpc
      , method
      , result
      , error
      , id
      }

-- OgmiosDecodeError -----------------------------------------------------------

data OgmiosDecodeError
  -- Server responded with error.
  = ErrorResponse (Maybe OgmiosError)
  -- Received JsonRpc2 error was not of the right format.
  | InvalidRpcError JsonDecodeError
  -- Received JsonRpc2 response was not of the right format.
  | InvalidRpcResponse JsonDecodeError

derive instance Generic OgmiosDecodeError _

instance Show OgmiosDecodeError where
  show = genericShow

pprintOgmiosDecodeError :: OgmiosDecodeError -> String
pprintOgmiosDecodeError =
  case _ of
    ErrorResponse err ->
      "Ogmios responded with error: " <>
        maybe "<Actually no response>" pprintOgmiosError err
    InvalidRpcError err ->
      "Ogmios error was not of the right format: "
        <> printJsonDecodeError err
    InvalidRpcResponse err ->
      "Ogmios response was not of the right format: "
        <> printJsonDecodeError err

ogmiosDecodeErrorToError :: OgmiosDecodeError -> Error
ogmiosDecodeErrorToError err = error $ pprintOgmiosDecodeError err

-- OgmiosError -----------------------------------------------------------------

newtype OgmiosError = OgmiosError
  { code :: Int
  , message :: String
  , data :: Maybe Aeson
  }

derive instance Generic OgmiosError _
derive instance Newtype OgmiosError _

instance Show OgmiosError where
  show = genericShow

instance DecodeAeson OgmiosError where
  decodeAeson =
    caseAesonObject (Left (TypeMismatch "Object")) \obj -> do
      code <- getField obj "code"
      message <- getField obj "message"
      dat <- getFieldOptional obj "data"
      pure $ OgmiosError { code, message, data: dat }

pprintOgmiosError :: OgmiosError -> String
pprintOgmiosError (OgmiosError err) = stringifyAeson $ encodeAeson err

-- OgmiosRedeemerPtr -----------------------------------------------------------

type OgmiosRedeemerPtr = { index :: UInt, purpose :: String }

redeemerTypeMismatch :: JsonDecodeError
redeemerTypeMismatch = TypeMismatch
  "Expected redeemer to be one of: \
  \(spend|mint|publish|withdraw|vote|propose)"

decodeRedeemerPointer :: OgmiosRedeemerPtr -> Either JsonDecodeError RedeemerPointer
decodeRedeemerPointer { index: redeemerIndex, purpose } =
  note redeemerTypeMismatch $ { redeemerTag: _, redeemerIndex } <$>
    redeemerTagFromString purpose

redeemerTagFromString :: String -> Maybe RedeemerTag
redeemerTagFromString = case _ of
  "spend" -> Just RedeemerTag.Spend
  "mint" -> Just RedeemerTag.Mint
  "publish" -> Just RedeemerTag.Cert
  "withdraw" -> Just RedeemerTag.Reward
  "vote" -> Just RedeemerTag.Vote
  "propose" -> Just RedeemerTag.Propose
  _ -> Nothing

-- TxEvaluationR ---------------------------------------------------------------

newtype TxEvaluationR = TxEvaluationR
  (Either TxEvaluationFailure TxEvaluationResult)

derive instance Newtype TxEvaluationR _
derive instance Generic TxEvaluationR _

instance Show TxEvaluationR where
  show = genericShow

instance DecodeOgmios TxEvaluationR where
  decodeOgmios =
    decodeErrorOrResult
      { parseError: map (wrap <<< Left) <<< decodeAeson }
      { parseResult: map (wrap <<< Right) <<< decodeAeson }

-- TxEvaluationResult ----------------------------------------------------------

newtype TxEvaluationResult = TxEvaluationResult
  (Map RedeemerPointer ExecutionUnits)

derive instance Newtype TxEvaluationResult _
derive instance Generic TxEvaluationResult _

instance Show TxEvaluationResult where
  show = genericShow

instance DecodeAeson TxEvaluationResult where
  decodeAeson =
    caseAesonArray (Left (TypeMismatch "Array"))
      ( map (TxEvaluationResult <<< Map.fromFoldable)
          <<< traverse decodeRdmrPtrExUnitsItem
      )
    where
    decodeRdmrPtrExUnitsItem
      :: Aeson
      -> Either JsonDecodeError (RedeemerPointer /\ ExecutionUnits)
    decodeRdmrPtrExUnitsItem elem = do
      res
        :: { validator :: OgmiosRedeemerPtr
           , budget :: { memory :: BigNum, cpu :: BigNum }
           } <- decodeAeson elem
      redeemerPtr <- decodeRedeemerPointer res.validator
      pure $ redeemerPtr /\ { memory: res.budget.memory, steps: res.budget.cpu }

-- TxEvaluationFailure ---------------------------------------------------------

-- The following cases are fine to fall through into unparsed error:
-- IncompatibleEra
-- NotEnoughSynced
-- CannotCreateEvaluationContext
data TxEvaluationFailure
  = UnparsedError String
  | AdditionalUtxoOverlap (Array OgmiosTxOutRef)
  | ScriptFailures (Map RedeemerPointer (Array ScriptFailure))

derive instance Generic TxEvaluationFailure _

instance Show TxEvaluationFailure where
  show = genericShow

instance DecodeAeson TxEvaluationFailure where
  decodeAeson aeson = do
    error :: OgmiosError <- decodeAeson aeson
    let code = (unwrap error).code
    errorData <- maybe (Left (AtKey "data" MissingValue)) pure
      (unwrap error).data
    case code of
      -- ScriptExecutionFailure
      3010 -> flip (caseAesonArray (Left (TypeMismatch "Array"))) errorData $
        ( \array ->
            ( ScriptFailures <<< map Array.fromFoldable <<< collectIntoMap <$>
                traverse parseElem array
            )
        )
      -- Overlapping AdditionalUtxo
      3002 -> do
        res
          :: { overlappingOutputReferences ::
                 Array { transaction :: { id :: String }, index :: UInt }
             } <- decodeAeson errorData
        pure $ AdditionalUtxoOverlap $ map
          (\elem -> { txId: elem.transaction.id, index: elem.index })
          res.overlappingOutputReferences
      -- All other errors
      _ -> pure $ UnparsedError $ stringifyAeson aeson

    where
    parseElem elem = do
      res :: { validator :: OgmiosRedeemerPtr, error :: ScriptFailure } <-
        decodeAeson elem
      (_ /\ res.error) <$> decodeRedeemerPointer res.validator

    collectIntoMap :: forall k v. Ord k => Array (k /\ v) -> Map k (List v)
    collectIntoMap = foldl
      ( \m (k /\ v) -> Map.alter
          (maybe (Just $ List.singleton v) (Just <<< List.Cons v))
          k
          m
      )
      Map.empty

-- ScriptFailure ---------------------------------------------------------------

-- | Reason a script failed.
--
-- FIXME: The type definition is a least common denominator between Ogmios v6 format used by ogmios backend
-- and ogmios v5.6 format used by blockfrost backend
data ScriptFailure
  = ExtraRedeemers (Array RedeemerPointer)
  | MissingRequiredDatums
      { missing :: (Array OgmiosDatum)
      , provided :: Maybe (Array OgmiosDatum)
      }
  | MissingRequiredScripts
      { missing :: Array RedeemerPointer
      , resolved :: Maybe (Map RedeemerPointer ScriptHash)
      }
  | ValidatorFailed { error :: String, traces :: Array String }
  | UnknownInputReferencedByRedeemer (Array OgmiosTxIn)
  | NonScriptInputReferencedByRedeemer OgmiosTxIn
  | NoCostModelForLanguage (Array String)
  | InternalLedgerTypeConversionError String
  | IllFormedExecutionBudget (Maybe ExecutionUnits)

derive instance Generic ScriptFailure _

instance Show ScriptFailure where
  show = genericShow

instance DecodeAeson ScriptFailure where
  decodeAeson aeson = do
    err :: OgmiosError <- decodeAeson aeson
    let error = unwrap err
    errorData <- maybe (Left (AtKey "data" MissingValue)) pure error.data
    case error.code of
      3011 -> do
        res :: { missingScripts :: Array OgmiosRedeemerPtr } <- decodeAeson
          errorData
        missing <- traverse decodeRedeemerPointer res.missingScripts
        pure $ MissingRequiredScripts { missing: missing, resolved: Nothing }
      3012 -> do
        res :: { validationError :: String, traces :: Array String } <-
          decodeAeson errorData
        pure $ ValidatorFailed
          { error: res.validationError, traces: res.traces }
      3013 -> do
        res
          :: { unsuitableOutputReference ::
                 { transaction :: { id :: String }, index :: Prim.Int }
             } <- decodeAeson errorData
        pure $ NonScriptInputReferencedByRedeemer
          { index: res.unsuitableOutputReference.index
          , txId: res.unsuitableOutputReference.transaction.id
          }
      3110 -> do
        res :: { extraneousRedeemers :: Array OgmiosRedeemerPtr } <- decodeAeson
          errorData
        ExtraRedeemers <$> traverse decodeRedeemerPointer
          res.extraneousRedeemers
      3111 -> do
        res :: { missingDatums :: Array String } <- decodeAeson errorData
        pure $ MissingRequiredDatums
          { missing: res.missingDatums, provided: Nothing }
      3117 -> do
        res
          :: { unknownOutputReferences ::
                 Array { transaction :: { id :: String }, index :: Prim.Int }
             } <- decodeAeson errorData
        pure $ UnknownInputReferencedByRedeemer $
          map (\x -> { index: x.index, txId: x.transaction.id })
            res.unknownOutputReferences
      3115 -> do
        res :: { missingCostModels :: Array String } <- decodeAeson errorData
        pure $ NoCostModelForLanguage res.missingCostModels
      -- this would actually fail at decoding error.data but it's good
      3999 -> pure $ InternalLedgerTypeConversionError error.message
      _ -> Left $ TypeMismatch $ "Unknown ogmios error code: " <> show
        error.code
