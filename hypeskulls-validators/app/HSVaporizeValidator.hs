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
import Plutus.V1.Ledger.Api (TokenName(unTokenName))

  

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> VaporizeDatum -> VaporizeAction -> ScriptContext -> Bool
mkValidator ContractInfo{..} datum r ctx = 
    case (datum, r) of
        (OrderDatum od, Vaporize)       ->  traceIfFalse "HYPESKULL not returned"                   (isOgSkullReturned od)      &&&
                                            traceIfFalse "New Sh Datum Invalid (1)"                 (isShHsReturnedProperly od)

        (ShadowHsDatum _, UpdateSh)     ->  traceIfFalse ""                                         True        

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

        originTn :: TokenName
        originTn = let !os = [ (cs, tn, n) | (cs, tn, n) <- Value.flattenValue ownInputValue, cs == ciOriginPolicy] in
                case os of
                    [(_, tn, _)]    -> tn
                    _               -> TokenName emptyByteString

        isOgSkullReturned :: VaporizeOrderDatum -> Bool
        isOgSkullReturned od =
            case returnedHs of
                [_] -> True 
                _   -> False
            where
                returnedHs = [   (cs, tn, n) 
                                    |   (cs, tn, n) <- Value.flattenValue $ valuePaidTo info $ vodPkh od
                                    ,   cs == ciOriginPolicy
                                    ,   tn == originTn
                                    ]
        
        isShHsReturnedProperly :: VaporizeOrderDatum -> Bool
        isShHsReturnedProperly od = 
            case getVaporizeDatum $ getContinuingShTxOut originTn of
                    Nothing                 -> False
                    Just (ShadowHSDatum (VaporizeOrderListDatum pkh addr _ _)) ->
                        ((pkh' == ciDefaultShadowHSOwner) ||| (pkh' == pkh))    &&&
                        ((addr' == ciDefaultShadowHSOwner) ||| (addr' == addr))
                    where
                        pkh'    = vodPkh od
                        addr'   = vodAddr od

        continuingOutputs :: [TxOut]
        !continuingOutputs = getContinuingOutputs ctx

        getContinuingShTxOut :: TokenName -> Maybe TxOut
        getContinuingShTxOut ogTn =
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
                                        !shTn = TokenName $ unTokenName ogTn P.<> ciShadowHSAffix

        getDatum :: TxOut -> Maybe Datum
        getDatum o = do
            dh <- txOutDatum o
            dh `findDatum` info

        getNewShDatum :: Maybe TxOut -> Maybe VaporizeDatum
        getNewShDatum o =
            case o of
                Nothing -> Nothing
                Just o'  -> do
                    Datum d' <- getDatum o'
                    PlutusTx.fromBuiltinData d'




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