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

module HSVaporizeValidator
    ( hsVaporizeSerialised
    , hsVaporizeSBS
    , hsVaporizeAddress
    , hsVaporizeInstance
    , hsVaporizeValidator
    , HSVaporize
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
import              HSVaporizeCommon


{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> VaporizeDatum -> VaporizeAction -> ScriptContext -> Bool
mkValidator ContractInfo{..} datum r ctx =
    case (datum, r) of
        (PTDatum _, Vaporize)       ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid ciPTTokenPrefix)       &&&
                                        traceIfFalse "Tx must have two validators"              hasTwoValidatorInputs               &&&
                                        traceIfFalse "Vaporization fees not paid"               isFeePaid                           &&&
                                        traceIfFalse "Vapor_PT token not returned properly"     isMrkrReturnedProperly              &&&
                                        traceIfFalse "New PT datum invalid"                     isNewPTDatumValid

        (ShadowHSDatum _, Vaporize) ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid ciShadowHSPrefix)      &&&
                                        traceIfFalse "Must spend at least 1 PT Token"           hasOnePTTokenSpent                  &&&
                                        traceIfFalse "No matching OS HYPESKULL sent to self"    hasMatchingOSHYPESKULL              &&&
                                        traceIfFalse "ShadowHS token not returned properly"     isMrkrReturnedProperly              &&&
                                        traceIfFalse "New SH datum invalid"                     isNewShadowHSDatumValid

        (ShadowHSDatum _, Deliver)  ->  traceIfFalse "Wrong input for this redeemer"            (isMrkrValid ciShadowHSPrefix)      &&&
                                        traceIfFalse "Tx Not signed by Admin"                   (txSignedBy info ciAdminPKH)        &&&
                                        traceIfFalse "New SH datum invalid"                     isNewShadowHSDatumValid'

        (_, Withdraw)               ->  traceIfFalse "Tx Not signed by Admin"                   (txSignedBy info ciAdminPKH)

        _                           ->  traceIfFalse "Invalid datum and redeemer pair"          False
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

            mrkrTN :: TokenName
            mrkrTN = let !os = [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue ownInputValue, cs == ciPolicy] in
                case os of
                    [(_, tn, _)]    -> tn
                    _               -> TokenName ""
            
            isMrkrValid :: BuiltinByteString -> Bool
            isMrkrValid bs = bs == P.sliceByteString 0 3 (unTokenName mrkrTN)

            isFeePaid :: Bool
            isFeePaid =
                case datum of
                    PTDatum price   -> 1_000_000 * price <= Ada.getLovelace (Ada.fromValue (valuePaidTo info ciAdminPKH))
                    _               -> False

            getContinuingMrkrTxOut :: Maybe TxOut
            getContinuingMrkrTxOut =
                case nftOuts of
                    [o]     -> Just o
                    _       -> Nothing
                where
                    nftOuts     =   [ o
                                    | o <- getContinuingOutputs ctx
                                    , 2 == length (Value.flattenValue (txOutValue o))
                                    , AssetCount (ciPolicy, mrkrTN, 1) `elem` [ AssetCount x | x <- Value.flattenValue (txOutValue o)]
                                    , Ada.getLovelace (Ada.fromValue(txOutValue o)) == ciMinUtxoLovelace ]


            getDatum :: TxOut -> Maybe Datum
            getDatum o = do
                dh <- txOutDatum o
                dh `findDatum` info

            getVaporizeDatum :: Maybe TxOut -> Maybe VaporizeDatum
            getVaporizeDatum o =
                case o of
                    Nothing -> Nothing
                    Just o'  -> do
                        Datum d' <- getDatum o'
                        PlutusTx.fromBuiltinData d'

            isMrkrReturnedProperly :: Bool
            isMrkrReturnedProperly =
                case getContinuingMrkrTxOut of
                    Nothing -> False
                    Just _  ->  True

            isNewPTDatumValid :: Bool
            isNewPTDatumValid =
                case getVaporizeDatum getContinuingMrkrTxOut of
                    Nothing                 -> False
                    Just (PTDatum newPrice) ->
                        case datum of
                            PTDatum price   -> price + 10 == newPrice
                            _               -> False

            isNewShadowHSDatumValid :: Bool
            isNewShadowHSDatumValid =
                case getVaporizeDatum getContinuingMrkrTxOut of
                    Nothing                 -> False
                    Just (ShadowHSDatum (VaporizeListDatum pkh vs)) ->
                        case datum of
                            ShadowHSDatum (VaporizeListDatum pkh' vs') -> 
                                ((pkh' == ciDefaultShadowHSOwner) ||| (pkh' == sig))    &&& 
                                (pkh == sig)                                            &&&
                                (length vs == length (nub vs))                          &&&
                                (length vs - length vs' == 1)                           &&&
                                case getDiff vs vs' of
                                    [v] ->  1 ==  assetClassValueOf (valuePaidTo info ciAdminPKH)
                                                (AssetClass (ciPolicy, TokenName $ ciVaporTokenName P.<> v))
                                    _   -> False
                            _   -> False

            isNewShadowHSDatumValid' :: Bool
            isNewShadowHSDatumValid' =
                case getVaporizeDatum getContinuingMrkrTxOut of
                    Nothing                 -> False
                    Just (ShadowHSDatum (VaporizeListDatum pkh vs)) ->
                        case datum of
                            ShadowHSDatum (VaporizeListDatum pkh' vs') -> 
                                (pkh' == pkh)                      &&&
                                (length vs == length (nub vs))     &&&
                                (length vs - length vs' == -1)     &&&
                                case getDiff vs' vs of
                                    [v] ->  1 ==  assetClassValueOf (valuePaidTo info vaporizeePKH)
                                            (AssetClass (ciPolicy, 
                                                TokenName $ P.sliceByteString 3 13 (unTokenName mrkrTN) P.<> "_" P.<> v))
                                    _   -> False
                            _   -> False

            getDiff :: Eq a => [a] -> [a] -> [a]
            getDiff xs [] = xs
            getDiff [] _ = []
            getDiff (x:xs) ys =  if x `elem` ys then getDiff xs ys else x:getDiff xs ys

            hasMatchingOSHYPESKULL :: Bool
            hasMatchingOSHYPESKULL =
                assetClassValueOf (valuePaidTo info sig) (AssetClass (ciPolicy, tn')) == 1
                    where tn' = TokenName $ P.sliceByteString 3 13 $ unTokenName mrkrTN

            hasTwoValidatorInputs :: Bool
            hasTwoValidatorInputs =
                let
                    validatorInputs :: [TxInInfo]
                    validatorInputs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
                in
                    length validatorInputs == 2

            hasOnePTTokenSpent :: Bool
            hasOnePTTokenSpent =
                let
                    ptTokenSpent :: [(CurrencySymbol, TokenName, Integer)]
                    ptTokenSpent =  [ (cs, tn, n)
                                    | (cs, tn, n) <- Value.flattenValue (valueSpent info)
                                    , cs == ciPolicy
                                    , ciPTTokenPrefix == P.sliceByteString 0 3 (unTokenName tn)
                                    , n == 1]
                in
                    length ptTokenSpent == 1

            vaporizeePKH :: PubKeyHash
            vaporizeePKH =
                case datum of
                    ShadowHSDatum (VaporizeListDatum pkh _)     -> pkh
                    _                                           -> PubKeyHash ""




data HSVaporize
instance Scripts.ValidatorTypes HSVaporize where
    type instance DatumType HSVaporize = VaporizeDatum
    type instance RedeemerType HSVaporize = VaporizeAction


hsVaporizeInstance :: ContractInfo -> Scripts.TypedValidator HSVaporize
hsVaporizeInstance ci = Scripts.mkTypedValidator @HSVaporize
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ci)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VaporizeDatum @VaporizeAction

hsVaporizeValidator :: Validator
hsVaporizeValidator = Scripts.validatorScript $ hsVaporizeInstance contractInfo

hsVaporizeScript :: Ledger.Script
hsVaporizeScript = unValidatorScript hsVaporizeValidator

hsVaporizeSBS :: SBS.ShortByteString
hsVaporizeSBS =  SBS.toShort . LBS.toStrict $ serialise hsVaporizeScript

hsVaporizeSerialised :: PlutusScript PlutusScriptV1
hsVaporizeSerialised = PlutusScriptSerialised hsVaporizeSBS

hsVaporizeAddress :: Ledger.Address
hsVaporizeAddress = scriptAddress hsVaporizeValidator