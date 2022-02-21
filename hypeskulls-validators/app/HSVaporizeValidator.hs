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
        (OrderDatum pkh, Vaporize)      ->  traceIfFalse "HYPESKULL not returned"                   (isOgSkullReturned pkh)         &&&
                                            traceIfFalse "New Sh Datum Invalid (1)"                 (isVaporizerDatumCorrect pkh)

        (ShadowHsDatum vld, UpdateSh)   ->  traceIfFalse "No PT token consumed"                     hasOnePTTokenSpent              &&&
                                            traceIfFalse "No matching OS HYPESKULL"                 hasMatchingOgHsSpent            &&&
                                            traceIfFalse "New Sh Datum Invalid (2)"                 (isOrderListUpdateCorrect vld)

        (PtDatum price, UsePt)          ->  traceIfFalse "Fees not paid"                            (isFeePaid price)               &&&
                                            traceIfFalse "New PT datum invalid"                     (isNewPTDatumValid price)
        
        (OrderDatum pkh, Refund)        ->  traceIfFalse "Tx Not signed by Admin"                   (txSignedBy info ciAdminPKH)    &&&
                                            traceIfFalse "Utxo Value not properly returned"         (isValueReturned pkh)
        
        (ShadowHsDatum vld, Deliver)    ->  traceIfFalse "Tx Not signed by Admin"                   (txSignedBy info ciAdminPKH)    &&&
                                            traceIfFalse "New Sh Datum Invalid (3)"                 (isOrderListUpdateCorrect' vld)

        (_, Withdraw)                   ->  traceIfFalse "Tx Not signed by Admin"                   (txSignedBy info ciAdminPKH)

        _                               ->  traceIfFalse "Unsupported datum and redeemer pair"      False
    
    where
        info :: TxInfo
        !info = scriptContextTxInfo ctx

        ownInputValue :: Value.Value
        !ownInputValue =
            case findOwnInput ctx of
                Nothing         -> Ada.lovelaceValueOf 1
                Just txInInfo   -> txOutValue $ txInInfoResolved txInInfo
        
        continuingOutputs :: [TxOut]
        !continuingOutputs = getContinuingOutputs ctx

        getDatum :: TxOut -> Maybe Datum
        getDatum o = do
            dh <- txOutDatum o
            dh `findDatum` info

        getUpdatedDatum :: Maybe TxOut -> Maybe VaporizeDatum
        getUpdatedDatum o =
            case o of
                Nothing -> Nothing
                Just o'  -> do
                    Datum d' <- getDatum o'
                    PlutusTx.fromBuiltinData d'

        originTn :: TokenName
        originTn =
            case os of
                [(_, tn, _)]    -> tn
                _               -> TokenName emptyByteString
                where   
                    os = filter f $ Value.flattenValue $ valueSpent info
                    f (cs, tn, n) = (cs == ciOriginPolicy) &&& (tn /= ciRessTokenName) &&& (n == 1)

        shTn :: TokenName
        shTn = 
            case os of
                [(_, tn, _)]    -> tn
                _               -> TokenName emptyByteString
                where   
                    os = filter f $ Value.flattenValue $ valueSpent info
                    f (cs, tn, n) = (cs == ciVaporPolicy) &&& 
                                    (n == 1)              &&&
                                    (ciShadowHSAffix == P.sliceByteString 13 (lengthOfByteString ciShadowHSAffix) (unTokenName tn))

        isOgSkullReturned :: PubKeyHash -> Bool
        isOgSkullReturned pkh =
            case returnedHs of
                [_] -> True 
                _   -> False
            where
                returnedHs =    [   (cs, tn, n) 
                                |   (cs, tn, n) <- Value.flattenValue $ valuePaidTo info pkh
                                ,   cs == ciOriginPolicy
                                ,   tn == originTn
                                ]
        
        isVaporizerDatumCorrect :: PubKeyHash -> Bool
        isVaporizerDatumCorrect pkh =
            case getUpdatedDatum getContinuingShTxOut of
                Nothing                 -> False
                Just (ShadowHsDatum (VaporizeListDatum pkh' _ _)) ->
                    (pkh == ciDefaultShadowHSOwner) ||| (pkh == pkh')

        getContinuingShTxOut :: Maybe TxOut
        getContinuingShTxOut =
                case nftOuts of
                    [o]     -> Just o
                    _       -> Nothing
                    where
                        nftOuts     = filter f continuingOutputs
                            where
                                f :: TxOut -> Bool
                                f o =   (2 == length flattenedVal)                                                          &&&
                                        (AssetCount (ciVaporPolicy, shTn, 1) `elem` [ AssetCount x | x <- flattenedVal])    &&&
                                        (Ada.getLovelace (Ada.fromValue txOutVal) == ciMinUtxoLovelace)
                                    where
                                        !txOutVal = txOutValue o
                                        !flattenedVal = Value.flattenValue txOutVal

        isOrderListUpdateCorrect :: VaporizeListDatum -> Bool
        isOrderListUpdateCorrect (VaporizeListDatum pkh os ds) = 
            case getUpdatedDatum getContinuingShTxOut of
                Nothing                 -> False
                Just (ShadowHsDatum (VaporizeListDatum _ os' ds')) ->
                    (ds' == ds)                                             &&&
                    (1 ==  assetClassValueOf (valuePaidTo info ciAdminPKH)
                        (AssetClass (ciVaporPolicy, TokenName $ ciVTAffix P.<> getVTName os os')))

        pow :: Integer -> Integer -> Integer    
        pow n e     = if e == 0 then 1 else 2 * pow n (e - 1)

        log2 :: Integer -> Integer
        log2 n      = if n == 1 then 0 else 1 + log2 (n `divide` 2) 

        toBinary:: Integer -> [Integer]
        toBinary n  = if n == 0 then [0] else modulo n 2 : toBinary (n `divide` 2)

        getVTName :: Integer -> Integer -> BuiltinByteString
        getVTName old new = 
            if (new > old) &&& isPowOf2 &&& isUnique
            then ciVaporTokenNames !! vtIdx
            else ciEmptyByteString
            where 
                !diff'       = new - old
                !vtIdx       = log2 diff'
                !oldBin      = toBinary old
                !oldBinLen   = length oldBin
                !isPowOf2    = pow 2 vtIdx == diff'
                !isUnique    = (oldBinLen <= vtIdx) ||| (0 == (oldBin !! vtIdx))
        
        hasMatchingOgHsSpent :: Bool
        hasMatchingOgHsSpent =
            length ogTokenSpent == 1
            where
                ogTokenSpent :: [(CurrencySymbol, TokenName, Integer)]
                ogTokenSpent =  filter f (Value.flattenValue (valueSpent info))
                    where
                        f (cs, tn, n) = (cs == ciOriginPolicy)  &&& 
                                        (tn == tn')             &&& 
                                        (n == 1)
                        tn' = TokenName $ P.sliceByteString 0 13 $ unTokenName shTn
        
        hasRessTokenSpent :: Bool
        hasRessTokenSpent =
            length ressTokenSpent == 1
            where
                ressTokenSpent :: [(CurrencySymbol, TokenName, Integer)]
                ressTokenSpent =  filter f (Value.flattenValue (valueSpent info))
                    where
                        f (cs, tn, n) = (cs == ciOriginPolicy)  &&& 
                                        (tn == ciRessTokenName) &&& 
                                        (n == 1)

        hasOnePTTokenSpent :: Bool
        hasOnePTTokenSpent = length ptTokenSpent == 1
            where
                ptTokenSpent :: [(CurrencySymbol, TokenName, Integer)]
                ptTokenSpent =  filter f (Value.flattenValue (valueSpent info))
                    where
                        f (cs, tn, n) = (cs == ciVaporPolicy)    &&& 
                                        (tn == ciPtTokenName)    &&& 
                                        (n == 1)

        isFeePaid :: Integer -> Bool
        isFeePaid price = Ada.getLovelace (Ada.fromValue (valuePaidTo info ciAdminPKH)) == price - ciMinUtxoLovelace - ciVaporizerFee - ressDiscount
            where 
                ressDiscount = if hasRessTokenSpent then ciRessDiscount else 0
        
        isNewPTDatumValid :: Integer -> Bool
        isNewPTDatumValid price =
            case getUpdatedDatum getContinuingPtTxOut of
                Nothing                 -> False
                Just (PtDatum newPrice) -> price + ciPriceTierDelta == newPrice

        getContinuingPtTxOut :: Maybe TxOut
        getContinuingPtTxOut =
            case nftOuts of
                [o]     -> Just o
                _       -> Nothing
                where
                    nftOuts     = filter f continuingOutputs
                        where
                            f :: TxOut -> Bool
                            f o =   (2 == length flattenedVal)                                                                  &&&
                                    (AssetCount (ciVaporPolicy, ciPtTokenName, 1) `elem` [ AssetCount x | x <- flattenedVal])   &&&
                                    (Ada.getLovelace (Ada.fromValue txOutVal) == ciMinUtxoLovelace)
                                where
                                    !txOutVal = txOutValue o
                                    !flattenedVal = Value.flattenValue txOutVal

        isValueReturned :: PubKeyHash -> Bool
        isValueReturned pkh = valuePaidTo info pkh == ownInputValue P.<> Ada.lovelaceValueOf (ciNegativeOne * ciVaporizerFee)

        getVTName' :: Integer -> Integer -> Integer -> BuiltinByteString
        getVTName' old' old new = 
            if (new > old) &&& isPowOf2 &&& isUnique &&& isPresent
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

        isOrderListUpdateCorrect' :: VaporizeListDatum -> Bool
        isOrderListUpdateCorrect' (VaporizeListDatum pkh os ds) = 
            case getUpdatedDatum getContinuingShTxOut of
                Nothing                 -> False
                Just (ShadowHsDatum (VaporizeListDatum pkh' os' ds')) ->
                    (os == os')                         &&&
                    (pkh' == pkh)                       &&&
                    (1 ==  assetClassValueOf (valuePaidTo info pkh)
                        (AssetClass ( ciVaporPolicy
                                    , TokenName $ P.sliceByteString 0 13 (unTokenName shTn)   P.<> 
                                    "_"                                                       P.<> 
                                    getVTName' os ds ds')))



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