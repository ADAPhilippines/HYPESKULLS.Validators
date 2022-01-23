{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
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
        (PTDatum _, Vaporize)       ->  traceIfFalse "Tx must have two validators"              hasTwoValidatorInputs               &&&
                                        traceIfFalse "Vaporization fees not paid"               isFeePaid                           &&&
                                        traceIfFalse "Vapor_PT token not returned properly"     isMrkrReturnedProperly              &&&
                                        traceIfFalse "New PT datum invalid"                     isNewPTDatumValid
                                        
        (ShadowHSDatum _, Vaporize) ->  traceIfFalse "Must spend at least 1 PT Token"           hasOnePTTokenSpent                  &&&
                                        traceIfFalse "No matching OS HYPESKULL sent to self"    hasMatchingOSHYPESKULL              &&&
                                        traceIfFalse "ShadowHS token not returned properly"     isMrkrReturnedProperly              &&&
                                        traceIfFalse "New SH datum invalid"                     isNewShadowHSDatumValid

        (ShadowHSDatum _, Deliver)  ->  traceIfFalse "Tx Not signed by Admin"                   (txSignedBy info ciAdminPKH)        &&&   
                                        traceIfFalse "New SH datum invalid"                     isNewShadowHSDatumValid'

        (_, Withdraw)               ->  traceIfFalse "Tx Not signed by Admin"                   (txSignedBy info ciAdminPKH)

        _                           ->  traceIfFalse "Invalid datum and redeemer pair"          False
        where
            info :: TxInfo
            !info = scriptContextTxInfo ctx

            sig :: PubKeyHash
            sig = case txInfoSignatories info of
                    [pkh] -> pkh
            
            continuingOutputs :: [TxOut]
            !continuingOutputs = getContinuingOutputs ctx

            mrkrTN :: TokenName
            mrkrTN = let !os = [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue (txOutValue (ownInput ctx)), cs == ciVaporPolicy] in
                case os of
                    [(_, tn, _)]    -> tn
                    _               -> TokenName ""

            isFeePaid :: Bool
            !isFeePaid =
                case datum of
                    PTDatum price   -> 1_000_000 * price <= Ada.getLovelace (Ada.fromValue (valuePaidTo info ciAdminPKH))
                    _               -> False

            getContinuingMrkrTxOut :: Maybe TxOut
            !getContinuingMrkrTxOut =
                case nftOuts of
                    [o]     -> Just o
                    _       -> Nothing
                where
                    nftOuts     = filter f continuingOutputs
                        where
                            f :: TxOut -> Bool
                            f o =   (2 == length flattenedVal)                                                          &&&
                                    (AssetCount (ciVaporPolicy, mrkrTN, 1) `elem` [ AssetCount x | x <- flattenedVal])  &&&
                                    (Ada.getLovelace (Ada.fromValue txOutVal) == ciMinUtxoLovelace)
                                where
                                    !txOutVal = txOutValue o
                                    !flattenedVal = Value.flattenValue txOutVal

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
                            PTDatum price   -> price + ciPriceTierDelta == newPrice
                            _               -> False

            pow :: Integer -> Integer -> Integer    
            pow n e     = if e == 0 then 1 else 2 * pow n (e - 1)

            log2 :: Integer -> Integer
            log2 n      = if n == 1 then 0 else 1 + log2 (n `divide` 2) 

            toBinary:: Integer -> [Integer]
            toBinary n  = if n == 0 then [0] else modulo n 2 : toBinary (n `divide` 2)

            getVTName :: Integer -> Integer -> BuiltinByteString
            getVTName old new = 
                if ((new > old) &&& isPowOf2 &&& isUnique)
                then ciVaporTokenNames !! vtIdx
                else ciEmptyByteString
                where 
                    !diff'       = new - old
                    !vtIdx       = log2 diff'
                    !oldBin      = toBinary old
                    !oldBinLen   = length oldBin
                    !isPowOf2    = pow 2 vtIdx == diff'
                    !isUnique    = (oldBinLen <= vtIdx) ||| (0 == (oldBin !! vtIdx))

            isNewShadowHSDatumValid :: Bool
            isNewShadowHSDatumValid =
                case getVaporizeDatum getContinuingMrkrTxOut of
                    Nothing                 -> False
                    Just (ShadowHSDatum (VaporizeListDatum _ os ds)) ->
                        case datum of
                            ShadowHSDatum (VaporizeListDatum pkh' os' ds') -> 
                                (ds' == ds)                                             &&& 
                                ((pkh' == ciDefaultShadowHSOwner) ||| (pkh' == sig))    &&&
                                (1 ==  assetClassValueOf (valuePaidTo info ciAdminPKH)
                                    (AssetClass (ciVaporPolicy, TokenName $ ciVTAffix P.<> (getVTName os' os))))
                            _   -> False
            
            getVTName' :: Integer -> Integer -> Integer -> BuiltinByteString
            getVTName' old' old new = 
                if ((new > old) &&& isPowOf2 &&& isUnique &&& isPresent)
                then ciVaporTokenNames !! vtIdx
                else ciEmptyByteString
                where 
                    !diff'       = new - old
                    !vtIdx       = log2 diff'
                    !oldBin      = toBinary old
                    !oldBin'     = toBinary old'
                    !oldBinLen   = length oldBin
                    !isPowOf2    = pow 2 vtIdx == diff'
                    !isUnique    = (oldBinLen <= vtIdx) ||| (0 == (oldBin !! vtIdx))
                    !isPresent   = 1 == oldBin' !! vtIdx

            isNewShadowHSDatumValid' :: Bool
            isNewShadowHSDatumValid' =
                case getVaporizeDatum getContinuingMrkrTxOut of
                    Nothing                 -> False
                    Just (ShadowHSDatum (VaporizeListDatum pkh os ds)) ->
                        case datum of
                            ShadowHSDatum (VaporizeListDatum pkh' os' ds') -> 
                                (os == os')                         &&&
                                (pkh' == pkh)                       &&&
                                (1 ==  assetClassValueOf (valuePaidTo info pkh)
                                    (AssetClass ( ciVaporPolicy
                                                , TokenName $ P.sliceByteString 0 13 (unTokenName mrkrTN)   P.<> 
                                                "_"                                                         P.<> 
                                                (getVTName' os' ds' ds))))
                            _   -> False

            hasMatchingOSHYPESKULL :: Bool
            hasMatchingOSHYPESKULL =
                assetClassValueOf (valuePaidTo info sig) (AssetClass (ciOriginPolicy, tn')) == 1
                    where tn' = TokenName $ P.sliceByteString 0 13 $ unTokenName mrkrTN

            hasTwoValidatorInputs :: Bool
            hasTwoValidatorInputs =
                let
                    validatorInputs :: [TxInInfo]
                    validatorInputs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
                in
                    length validatorInputs == 2

            hasOnePTTokenSpent :: Bool
            hasOnePTTokenSpent = length ptTokenSpent == 1
                where
                    ptTokenSpent :: [(CurrencySymbol, TokenName, Integer)]
                    ptTokenSpent =  filter f (Value.flattenValue (valueSpent info))
                        where
                            f (cs, tn, n) = (cs == ciVaporPolicy)               &&& 
                                            (tn == TokenName ciPTTokenAffix)    &&& 
                                            (n == 1)
                    

            ownInput :: ScriptContext -> TxOut
            ownInput (ScriptContext t_info (Spending o_ref)) = getScriptInput (txInfoInputs t_info) o_ref
            ownInput _ = traceError "script input not found !!!"

            getScriptInput :: [TxInInfo] -> TxOutRef -> TxOut
            getScriptInput [] _ = traceError "script input not found !!!"
            getScriptInput ((TxInInfo tref ot) : tl) o_ref
                | tref == o_ref = ot
                | otherwise = getScriptInput tl o_ref
                



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