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
mkValidator :: ContractInfo -> VTClaimDatum -> VTClaimAction -> ScriptContext -> P.Bool
mkValidator ContractInfo{..} d r ctx =
    case (d, r, nftMark) of
        (CommitSkullDatum, CommitSkull, Just ciShadowHSPrefix)      ->  traceIfFalse "No matching OS HYPESKULL in input"    hasMatchingOSHYPESKULL      &&&
                                                                        traceIfFalse "Shadow HYPESKULL not disposed"        isShadowHSDisposed          &&&
                                                                        traceIfFalse "Minimum lovelace not returned"        isMinUtxoLovelaceReturned

        (CommitRandomDatum dat, CommitRandom, Just ciVTRandPrefix)  ->  traceIfFalse "VTRandom UTXO not available"          (isVTRandUtxoAvailable dat) &&&
                                                                        traceIfFalse "VTRandom Token not returned properly" isVTRReturnedProperly

        _                                                           ->  traceIfFalse "Datum and redeemer does not match"    False
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

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

        nftMark :: Maybe BuiltinByteString
        nftMark = 
            case markerTN of
                Nothing     -> Nothing
                Just tn     -> Just $ P.sliceByteString 0 3 $ unTokenName tn

        hasMatchingOSHYPESKULL :: Bool
        hasMatchingOSHYPESKULL = 
            case markerTN of
                Nothing     -> traceIfFalse "Invalid input utxo selected" False
                Just tn     -> assetClassValueOf (valueSpent info) (AssetClass (ciPolicy, tn')) == 1
                    where tn' = TokenName $ P.sliceByteString 3 13 $ unTokenName tn

        isShadowHSDisposed :: Bool
        isShadowHSDisposed = 
            case markerTN of
                Nothing     -> traceIfFalse "Invalid input utxo selected" False
                Just tn     -> assetClassValueOf (valuePaidTo info ciAdminPKH) (AssetClass (ciPolicy,tn)) == 1

        totalNFTsReturned :: Integer
        totalNFTsReturned = length [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue (valuePaidTo info ciAdminPKH), cs == ciPolicy]

        isMinUtxoLovelaceReturned :: Bool
        isMinUtxoLovelaceReturned = totalNFTsReturned * ciMinUtxoLovelace <= Ada.getLovelace (Ada.fromValue $ valuePaidTo info ciAdminPKH)

        isVTRandUtxoAvailable :: VTDatum -> Bool
        isVTRandUtxoAvailable datum = vtdOwner datum == ciDefaultVTRandOwner

        isVTRReturnedProperly :: Bool
        isVTRReturnedProperly = 
            case markerTN of
                Nothing -> False
                Just tn -> isReturned
                    where
                        nftOuts = [ o 
                                  | o <- getContinuingOutputs ctx, 
                                    2 == length (Value.flattenValue (txOutValue o)), 
                                    (ciPolicy, tn, 1) `elem` Value.flattenValue (txOutValue o)]
                        isReturned = case nftOuts of
                            [_] -> True
                            _   -> False



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