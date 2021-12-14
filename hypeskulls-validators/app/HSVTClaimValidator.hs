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
{-# LANGUAGE BangPatterns           #-}
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
import              HSVTClaimCommon
import Plutus.V1.Ledger.Ada (adaSymbol)


{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> VTClaimDatum -> VTClaimAction -> ScriptContext -> Bool
mkValidator ContractInfo{..} datum r ctx =
    case (datum, r) of
        (ShadowHSDatum, CommitSkull)    ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid ciShadowHSPrefix)                  &&&
                                            traceIfFalse "No matching OS HYPESKULL  sent to self"   hasMatchingOSHYPESKULL                          &&&
                                            traceIfFalse "Shadow HYPESKULL not disposed"            isMarkerNFTDisposed                             &&&
                                            traceIfFalse "Insufficient Resurrection Tokens"         isHSRTAmountCorrect                             &&&
                                            traceIfFalse "Minimum lovelace not returned"            (isMinUtxoLovelaceReturned totalNFTsReturned)

        (VTRDatum _, CommitRandom)      ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid ciVTRandPrefix)                    &&&
                                            traceIfFalse "VTRandom UTXO not available"              isVTRandUtxoAvailable                           &&&
                                            traceIfFalse "VTRandom Token not returned properly"     isVTRReturnedProperly                           &&&
                                            traceIfFalse "Invalid Datum"                            isVTRCommitDatumValid                           &&&
                                            traceIfFalse "Wrong amount of VTR tokens committed"     isVTRCommitTokenAmountCorrect

        (VTRDatum _, UseRandom)         ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid ciVTRandPrefix)                    &&&
                                            traceIfFalse "Spender not allowed"                      isVTRSpendingAllowed                            &&&
                                            traceIfFalse "VTR Token not disposed"                   isMarkerNFTDisposed                             &&&
                                            traceIfFalse "Minimum lovelace not returned"            (isMinUtxoLovelaceReturned totalNFTsUsed)

        (VTDatum _, ClaimVT)            ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid ciVTPrefix)                        &&&
                                            traceIfFalse "Not allowed to claim VT"                  canClaimVT


        (_, Withdraw)                   ->  traceIfFalse "Tx Not signed by Admin"                   (txSignedBy info ciAdminPKH)

        _                               ->  traceIfFalse "Unsupported datum and redeemer pair"      False

    where
        info :: TxInfo
        !info = scriptContextTxInfo ctx

        sig :: PubKeyHash
        sig = case txInfoSignatories info of
                [pkh] -> pkh

        ownInputValue :: Value.Value
        !ownInputValue =
            case findOwnInput ctx of
                Nothing         -> Ada.lovelaceValueOf 1
                Just txInInfo   -> txOutValue $ txInInfoResolved txInInfo

        mrkrTN :: TokenName
        mrkrTN = let !os = [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue ownInputValue, cs == ciPolicy] in
                case os of
                    [(_, tn, _)]    -> tn
                    _               -> TokenName emptyByteString

        isMrkrValid :: BuiltinByteString -> Bool
        isMrkrValid bs = bs == P.sliceByteString 0 3 (unTokenName mrkrTN)

        hasMatchingOSHYPESKULL :: Bool
        hasMatchingOSHYPESKULL = assetClassValueOf (valuePaidTo info sig) (AssetClass (ciPolicy, tn')) == 1
                    where tn' = TokenName $ P.sliceByteString 3 13 $ unTokenName mrkrTN

        isMarkerNFTDisposed :: Bool
        isMarkerNFTDisposed = assetClassValueOf (valuePaidTo info ciAdminPKH) (AssetClass (ciPolicy, mrkrTN)) == 1

        totalNFTsReturned :: Integer
        totalNFTsReturned = length  [   (cs, tn, n) 
                                    |   (cs, tn, n) <- Value.flattenValue (valuePaidTo info ciAdminPKH)
                                    ,   cs == ciPolicy
                                    ,   ciShadowHSPrefix == P.sliceByteString 0 3 (unTokenName tn)
                                    ]

        totalNFTsUsed :: Integer
        totalNFTsUsed = length      [   (cs, tn, n) 
                                    |   (cs, tn, n) <- Value.flattenValue (valueSpent info)
                                    ,   cs == ciPolicy
                                    ,   (ciVTRandPrefix == P.sliceByteString 0 3 (unTokenName tn))      |||
                                        (ciShadowHSPrefix == P.sliceByteString 0 3 (unTokenName tn))    |||
                                        ("_VT_" == P.sliceByteString 10 4 (unTokenName tn))
                                    ]

        isMinUtxoLovelaceReturned :: Integer -> Bool
        isMinUtxoLovelaceReturned n = n * ciMinUtxoLovelace <= Ada.getLovelace (Ada.fromValue $ valuePaidTo info ciAdminPKH)

        isVTRandUtxoAvailable :: Bool
        isVTRandUtxoAvailable =
            case datum of
                VTRDatum pkh    -> pkh == ciDefaultVTRandOwner
                _               -> False

        getContinuingMarkerTxOut :: Maybe TxOut
        getContinuingMarkerTxOut = 
            case nftOuts of
                [o]     -> Just o
                _       -> Nothing
                where
                    nftOuts     =   [ o
                                    | o <- getContinuingOutputs ctx
                                    ,   let !outVal = txOutValue o 
                                            !flatVal = Value.flattenValue outVal
                                        in 
                                        (2 == length flatVal) &&&
                                        (AssetCount (ciPolicy, mrkrTN, 1) `elem` [ AssetCount x | x <- flatVal]) &&&
                                        (AssetCount (adaSymbol, Ada.adaToken, ciMinUtxoLovelace) `elem` [ AssetCount x | x <- flatVal])
                                    ]
                        

        isVTRReturnedProperly :: Bool
        isVTRReturnedProperly =
            case getContinuingMarkerTxOut of
                Nothing -> False
                Just _  ->  True

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
        isVTRCommitTokenAmountCorrect = length vtrNFTsSpent == 2 * totalNFTsReturned
            where
                vtrNFTsSpent   =   [ (cs, tn, n)
                                    | (cs, tn, n) <- Value.flattenValue (valueSpent info)
                                    , cs == ciPolicy
                                    , ciVTRandPrefix == P.sliceByteString 0 3 (unTokenName tn)]

        isHSRTAmountCorrect :: Bool
        isHSRTAmountCorrect = totalNFTsReturned == assetClassValueOf (valuePaidTo info ciAdminPKH) (AssetClass (ciPolicy, ciHSResurrectionTN))

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
                        matchingVTR = filter (\(_,tn,_) -> hash == sha2_256 (unTokenName tn P.<> "_" P.<> ciNonce)) hypeNFTsSpent
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