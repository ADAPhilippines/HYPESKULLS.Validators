{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
-- This example is taken directly from cardano-api, written by Jordan Millar, IOHK

module HSVTClaimValidator
    ( hsVTClaimSerialised
    , hsVTClaimSBS
    , hsVTClaimAddress
    , hsVTClaimInstance
    , hsVTClaimValidator
    , HSVTClaim
    ) where

import              Cardano.Api.Shelley     (PlutusScript (..))
import              Codec.Serialise
import qualified    Data.ByteString.Lazy    as LBS
import qualified    Data.ByteString.Short   as SBS
import              Ledger
import              Ledger.Value            as Value
import qualified    PlutusTx.Prelude        as P
import qualified    Ledger.Typed.Scripts    as Scripts
import qualified    PlutusTx
import              PlutusTx.Prelude        hiding (Semigroup (..), unless)
import              Cardano.Api             ( PlutusScriptV1 )
import qualified    Plutus.V1.Ledger.Ada    as Ada
import              PlutusTx.Skeleton

import              HSVTClaimCommon


{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> VTClaimDatum -> VTClaimAction -> ScriptContext -> Bool
mkValidator ContractInfo{..} datum r ctx =
    case (datum, r) of
        (ShadowHSDatum, CommitSkull)    ->  traceIfFalse "Wrong input for this redeemer"        (isMarkerValid ciShadowHSPrefix)    &&&
                                            traceIfFalse "No matching OS HYPESKULL in input"    hasMatchingOSHYPESKULL              &&&
                                            traceIfFalse "Shadow HYPESKULL not disposed"        isMarkerNFTDisposed                 &&&
                                            traceIfFalse "Minimum lovelace not returned"        isMinUtxoLovelaceReturned

        (VTRDatum _, CommitRandom)      ->  traceIfFalse "Wrong input for this redeemer"        (isMarkerValid ciVTRandPrefix)      &&&
                                            traceIfFalse "VTRandom UTXO not available"          isVTRandUtxoAvailable               &&&
                                            traceIfFalse "VTRandom Token not returned properly" isVTRReturnedProperly               &&&
                                            traceIfFalse "Invalid Datum"                        isVTRCommitDatumValid               &&&
                                            traceIfFalse "Wrong amount of VTR tokens committed" isVTRCommitTokenAmountCorrect

        (VTRDatum _, UseRandom)         ->  traceIfFalse "Wrong input for this redeemer"        (isMarkerValid ciVTRandPrefix)      &&&
                                            traceIfFalse "Spender not allowed"                  isVTRSpendingAllowed                &&&
                                            traceIfFalse "VTR Token not disposed"               isMarkerNFTDisposed                 &&&
                                            traceIfFalse "Minimum lovelace not returned"        isMinUtxoLovelaceReturned

        (VTDatum _, ClaimVT)            ->  traceIfFalse "Wrong input for this redeemer"        (isMarkerValid ciVTPrefix)          &&&
                                            traceIfFalse "Not allowed to claim VT"              canClaimVT

        _                               ->  traceIfFalse "Datum and redeemer does not match"    False

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        sig :: PubKeyHash
        sig = case txInfoSignatories info of
                [pkh] -> pkh

        ownInputValue :: Value.Value
        ownInputValue =
            case findOwnInput ctx of
                Nothing         -> Ada.lovelaceValueOf 1
                Just txInInfo   -> txOutValue $ txInInfoResolved txInInfo

        markerTN :: Maybe TokenName
        markerTN = let os = [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue ownInputValue, cs == ciPolicy] in
                case os of
                    [(_, tn, _)]    -> Just tn
                    _               -> Nothing

        isMarkerValid :: BuiltinByteString -> Bool
        isMarkerValid bs =
            case markerTN of
                Nothing     -> traceIfFalse "Invalid input utxo selected" False
                Just tn     -> bs == P.sliceByteString 0 3 (unTokenName tn)

        hasMatchingOSHYPESKULL :: Bool
        hasMatchingOSHYPESKULL =
            case markerTN of
                Nothing     -> traceIfFalse "Invalid input utxo selected" False
                Just tn     -> assetClassValueOf (valueSpent info) (AssetClass (ciPolicy, tn')) == 1
                    where tn' = TokenName $ P.sliceByteString 3 13 $ unTokenName tn

        isMarkerNFTDisposed :: Bool
        isMarkerNFTDisposed =
            case markerTN of
                Nothing     -> traceIfFalse "Invalid input utxo selected" False
                Just tn     -> assetClassValueOf (valuePaidTo info ciAdminPKH) (AssetClass (ciPolicy,tn)) == 1

        totalNFTsReturned :: Integer
        totalNFTsReturned = length [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue (valuePaidTo info ciAdminPKH), cs == ciPolicy]

        isMinUtxoLovelaceReturned :: Bool
        isMinUtxoLovelaceReturned = totalNFTsReturned * ciMinUtxoLovelace <= Ada.getLovelace (Ada.fromValue $ valuePaidTo info ciAdminPKH)

        isVTRandUtxoAvailable :: Bool
        isVTRandUtxoAvailable =
            case datum of
                VTRDatum pkh    -> pkh == ciDefaultVTRandOwner
                _               -> False

        getContinuingMarkerTxOut :: Maybe TxOut
        getContinuingMarkerTxOut =
            case markerTN of
                Nothing -> Nothing
                Just tn -> markerOut
                    where
                        nftOuts     =   [ o
                                        | o <- getContinuingOutputs ctx
                                        , 2 == length (Value.flattenValue (txOutValue o))
                                        , AssetCount (ciPolicy, tn, 1) `elem` [ AssetCount x| x <- Value.flattenValue (txOutValue o)]]
                        markerOut   = case nftOuts of
                            [o]     -> Just o
                            _       -> Nothing

        isVTRReturnedProperly :: Bool
        isVTRReturnedProperly =
            case getContinuingMarkerTxOut of
                Nothing -> False
                Just _  -> True

        getDatum :: TxOut -> Maybe Datum
        getDatum o = do
            dh <- txOutDatum o
            dh `findDatum` info

        getVTClaimDatum :: Maybe TxOut -> Maybe VTClaimDatum
        getVTClaimDatum o =
            case o of
                Nothing -> Nothing
                Just o'  -> do
                    Datum d' <- getDatum o'
                    PlutusTx.fromBuiltinData d'

        isVTRCommitDatumValid :: Bool
        isVTRCommitDatumValid =
            case getVTClaimDatum getContinuingMarkerTxOut of
                Nothing             -> False
                Just (VTRDatum pkh) -> pkh == sig

        isVTRCommitTokenAmountCorrect :: Bool
        isVTRCommitTokenAmountCorrect = length vtrNFTsSpent == 2 * length shNFTsSpent
            where
                hypeNFTsSpent = [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue (valueSpent info), cs == ciPolicy]
                vtrNFTsSpent = filter (\(_,tn,_) -> ciVTRandPrefix == P.sliceByteString 0 3 (unTokenName tn)) hypeNFTsSpent
                shNFTsSpent = filter (\(_,tn,_) -> ciShadowHSPrefix == P.sliceByteString 0 3 (unTokenName tn)) hypeNFTsSpent

        isVTRSpendingAllowed :: Bool
        isVTRSpendingAllowed =
            case datum of
                VTRDatum pkh    -> txSignedBy info pkh
                _               -> False

        getMatchingVTRTN :: Maybe TokenName
        getMatchingVTRTN =
            case datum of
                VTDatum hash -> mbTN
                    where
                        hypeNFTsSpent = [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue (valueSpent info), cs == ciPolicy]
                        matchingVTR = filter (\(_,tn,_) -> hash == sha2_256 (ciNonce `appendByteString` unTokenName tn)) hypeNFTsSpent
                        mbTN =
                            case matchingVTR of
                                [(_,tn',_)] -> Just tn'
                                _           -> Nothing
                _                           -> Nothing

        getInputTxOutWithVTR :: Maybe TxOut
        getInputTxOutWithVTR =
            case getMatchingVTRTN of
                Nothing -> Nothing
                Just tn ->
                    case [ o | TxInInfo _ o <- txInfoInputs info, valueOf (txOutValue o) ciPolicy tn == 1 ] of
                        [o] -> Just o
                        _   -> Nothing

        canClaimVT :: Bool
        canClaimVT =
            case getVTClaimDatum getInputTxOutWithVTR of
                Nothing             -> False
                Just (VTRDatum pkh) -> pkh == sig



data HSVTClaim
instance Scripts.ValidatorTypes HSVTClaim where
    type instance DatumType HSVTClaim = VTClaimDatum
    type instance RedeemerType HSVTClaim = VTClaimAction


hsVTClaimInstance :: ContractInfo -> Scripts.TypedValidator HSVTClaim
hsVTClaimInstance ci = Scripts.mkTypedValidator @HSVTClaim
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ci)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VTClaimDatum @VTClaimAction

hsVTClaimValidator :: Validator
hsVTClaimValidator = Scripts.validatorScript $ hsVTClaimInstance contractInfo

hsVTClaimScript :: Ledger.Script
hsVTClaimScript = unValidatorScript hsVTClaimValidator

hsVTClaimSBS :: SBS.ShortByteString
hsVTClaimSBS =  SBS.toShort . LBS.toStrict $ serialise hsVTClaimScript

hsVTClaimSerialised :: PlutusScript PlutusScriptV1
hsVTClaimSerialised = PlutusScriptSerialised hsVTClaimSBS

hsVTClaimAddress :: Ledger.Address
hsVTClaimAddress = scriptAddress hsVTClaimValidator