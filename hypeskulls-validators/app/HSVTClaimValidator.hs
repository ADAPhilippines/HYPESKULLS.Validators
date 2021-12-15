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


{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> VTClaimDatum -> VTClaimAction -> ScriptContext -> Bool
mkValidator ContractInfo{..} datum r ctx =
    case (datum, r) of
        (ShadowHSDatum, CommitSkull)    ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid 13 ciShadowHSAffix)                    &&&
                                            traceIfFalse "No matching OS HYPESKULL  sent to self"   hasMatchingOSHYPESKULL                              &&&
                                            traceIfFalse "Shadow HYPESKULL not disposed"            isMarkerNFTDisposed                                 &&&
                                            traceIfFalse "Tx must have two validators"              hasTwoValidatorInputs                               &&&
                                            traceIfFalse "Minimum lovelace not returned"            (isMinUtxoLovelaceReturned 1)

        (VRTDatum _, CommitRandom)      ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid 0 ciVRTAffix)                          &&&
                                            traceIfFalse "No Shadow HS disposed"                    isShadowHSDisposed                                  &&&
                                            traceIfFalse "VRT UTXO not available"                   isVRTUtxoAvailable                                  &&&
                                            traceIfFalse "VRT Token not returned properly"          isVRTReturnedProperly                               

        (VRTDatum _, ProveOwner)        ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid 0 ciVRTAffix)                          &&&
                                            traceIfFalse "Spender not allowed"                      isVRTSpendingAllowed                                &&&
                                            traceIfFalse "VRT Token not returned properly"          isVRTReturnedProperly       

        (VRTDatum _, UseRandom)         ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid 0 ciVRTAffix)                          &&&
                                            traceIfFalse "Spender not allowed"                      isVRTSpendingAllowed                                &&&
                                            traceIfFalse "VRT Token not disposed"                   isMarkerNFTDisposed                                 &&&
                                            traceIfFalse "Minimum lovelace not returned"            (isMinUtxoLovelaceReturned totalNFTsUsed)

        (VTDatum _, ClaimVT)            ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid 0 ciVTAffix)                           &&&
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

        isMrkrValid :: Integer -> BuiltinByteString -> Bool
        isMrkrValid startIdx bs = bs == P.sliceByteString startIdx (lengthOfByteString bs) (unTokenName mrkrTN)

        hasMatchingOSHYPESKULL :: Bool
        hasMatchingOSHYPESKULL = assetClassValueOf (valuePaidTo info sig) (AssetClass (ciPolicy, tn')) == 1
                    where tn' = TokenName $ P.sliceByteString 0 13 $ unTokenName mrkrTN

        isMarkerNFTDisposed :: Bool
        isMarkerNFTDisposed = assetClassValueOf (valuePaidTo info ciAdminPKH) (AssetClass (ciPolicy, mrkrTN)) == 1

        totalNFTsUsed :: Integer
        totalNFTsUsed = length      [   (cs, tn, n) 
                                    |   (cs, tn, n) <- Value.flattenValue (valueSpent info)
                                    ,   let
                                            lenVRTAffix     = lengthOfByteString ciVRTAffix
                                            lenShadowAffix  = lengthOfByteString ciShadowHSAffix
                                            lenVTAffix  = lengthOfByteString ciVTAffix
                                        in
                                        (cs == ciPolicy)                                                            &&&
                                        ((ciVRTAffix == P.sliceByteString 0 lenVRTAffix (unTokenName tn))           |||
                                        (ciShadowHSAffix == P.sliceByteString 13 lenShadowAffix (unTokenName tn))   |||
                                        (ciVTAffix == P.sliceByteString 0 lenVTAffix (unTokenName tn)))
                                    ]

        isMinUtxoLovelaceReturned :: Integer -> Bool
        isMinUtxoLovelaceReturned n = n * ciMinUtxoLovelace <= Ada.getLovelace (Ada.fromValue $ valuePaidTo info ciAdminPKH)

        isVRTUtxoAvailable :: Bool
        isVRTUtxoAvailable =
            case datum of
                VRTDatum pkh    -> pkh == ciDefaultVRTOwner
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
                                        (AssetCount (Ada.adaSymbol, Ada.adaToken, ciMinUtxoLovelace) `elem` [ AssetCount x | x <- flatVal])
                                    ]

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

        isVRTReturnedProperly :: Bool
        isVRTReturnedProperly =
            case getVTClaimDatum getContinuingMarkerTxOut of
                Nothing             -> False
                Just (VRTDatum pkh) -> pkh == sig
        
        isShadowHSDisposed :: Bool
        isShadowHSDisposed = 
            case disposedShadowHSs of
                [_] -> True 
                _   -> False
            where
                disposedShadowHSs = [   (cs, tn, n) 
                                    |   (cs, tn, n) <- Value.flattenValue (valuePaidTo info ciAdminPKH)
                                    ,   cs == ciPolicy
                                    ,   ciShadowHSAffix == P.sliceByteString 13 (lengthOfByteString ciShadowHSAffix) (unTokenName tn)
                                    ]

        isVRTSpendingAllowed :: Bool
        isVRTSpendingAllowed =
            case datum of
                VRTDatum pkh    -> txSignedBy info pkh
                _               -> False

        getMatchingVRTTN :: Maybe TokenName
        getMatchingVRTTN =
            case datum of
                VTDatum hash -> mbTN
                    where
                        hypeNFTsSpent = [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue (valuePaidTo info ciAdminPKH), cs == ciPolicy]
                        matchingVRT = filter (\(_,tn,_) -> hash == sha2_256 (unTokenName tn P.<> "_" P.<> ciNonce)) hypeNFTsSpent
                        mbTN =
                            case matchingVRT of
                                [(_,tn',_)] -> Just tn'
                                _           -> Nothing
                _                           -> Nothing

        getInputTxOutWithVRT :: Maybe TxOut
        getInputTxOutWithVRT =
            case getMatchingVRTTN of
                Nothing -> Nothing
                Just tn ->
                    case [ o | TxInInfo _ o <- txInfoInputs info, valueOf (txOutValue o) ciPolicy tn == 1 ] of
                        [o] -> Just o
                        _   -> Nothing

        canClaimVT :: Bool
        canClaimVT =
            case getVTClaimDatum getInputTxOutWithVRT of
                Nothing             -> False
                Just (VRTDatum pkh) -> pkh == sig
        
        hasTwoValidatorInputs :: Bool
        hasTwoValidatorInputs =
                let
                    validatorInputs :: [TxInInfo]
                    validatorInputs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
                in
                    length validatorInputs == 2



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