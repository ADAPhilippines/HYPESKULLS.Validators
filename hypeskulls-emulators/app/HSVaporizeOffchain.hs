{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE NumericUnderscores     #-}


module HSVaporizeOffchain
    ( endpoints
    , SetupParams (..)
    , VaporizeParams (..)
    , DeliverParams (..)
    ) where

import qualified    PlutusTx
import              PlutusTx.Builtins
import qualified    PlutusTx.Prelude        as P
import              Plutus.Contract         as Contract
import              Ledger
import              Ledger.Constraints      as Constraints
import              Ledger.Value            as Value
import qualified    Ledger.Ada              as Ada
import              Prelude                 as H
import              Data.Map                as Map
import              Data.Text
import              Control.Monad       
import              Data.Aeson                
import              GHC.Generics            

import              HSVaporizeCommon
import              HSVaporizeValidator


setupContract :: (AsContractError e) => SetupParams -> Contract w s e ()
setupContract params = do
    let minUtxoLovelace =   ciMinUtxoLovelace contractInfo
        nftUtxoVal tn   =   assetClassValue (AssetClass (ciVaporPolicy contractInfo, tn)) 1 <> Ada.lovelaceValueOf minUtxoLovelace
        tx              =   mconcat[Constraints.mustPayToTheScript d (nftUtxoVal tn') | (tn', d) <- spShadowUtxos params]       
                        <>  mconcat[Constraints.mustPayToTheScript d (nftUtxoVal tn') | (tn', d) <- spPriceTierUtxos params]    
                        <>  mconcat[Constraints.mustPayToTheScript d v                | (v, d) <- spOrderUtxos params]
    ledgerTx <- submitTxConstraints (hsVaporizeInstance contractInfo) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "set up contract address"


findScriptUtxos :: (AsContractError e) => Integer -> BuiltinByteString -> Contract w s e [(TxOutRef, ChainIndexTxOut, AssetClass)]
findScriptUtxos startIdx affix = do
    utxos <- utxosAt hsVaporizeAddress 
    return [ (oref, o, AssetClass (hypePolicyId, tn o)) | (oref, o) <- Map.toList utxos, tn o /= ""]
    where
        hypePolicyId = ciVaporPolicy contractInfo
        utxoAssets utxo = [ (cs, tn', n) | (cs, tn', n) <- Value.flattenValue (txOutValue $ toTxOut utxo), cs == hypePolicyId]
        tn utxo =
            case utxoAssets utxo of
                [(_,tn',_)]    -> if isRightAffix tn' then tn' else ""
                _               -> ""
        isRightAffix tn' = affix == P.sliceByteString startIdx (lengthOfByteString affix) (unTokenName tn')

logUtxos :: (AsContractError e) => Contract w s e ()
logUtxos = do
    ptUtxos <- findScriptUtxos 0 $ unTokenName $ ciPtTokenName contractInfo
    shUtxos <- findScriptUtxos 13 $ ciShadowHSAffix contractInfo
    odUtxos <- findScriptUtxos 0 $ ciVTAffix contractInfo
    logInfo @String $ "found pt utxos: " P.++ show (P.length ptUtxos)
    logInfo @String $ "found sh utxos: " P.++ show (P.length shUtxos)
    logInfo @String $ "found order utxos: " P.++ show (P.length odUtxos)

vaporize :: (AsContractError e) => VaporizeParams -> Contract w s e ()
vaporize params = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    ptUtxos <- findScriptUtxos 0 $ unTokenName $ ciPtTokenName contractInfo
    shUtxos <- findScriptUtxos 13 $ ciShadowHSAffix contractInfo
    odUtxos <- findScriptUtxos 0 $ ciVTAffix contractInfo
    case (shUtxos, ptUtxos, odUtxos) of
        ([],[],[])  -> logInfo @String "No utxos at script address"
        (shUtxos', ptUtxos', odUtxos')  -> do
            let amt             =   vpPrice params
                vaporizerPkh    =   vpVaporizerPkh params
                minUtxoLovelace =   ciMinUtxoLovelace contractInfo
                vaporizerFee    =   ciVaporizerFee contractInfo
                ressDiscount    =   ciRessDiscount contractInfo
                adminPkh        =   ciAdminPKH contractInfo
                originPolicy    =   ciOriginPolicy contractInfo
                vaporPolicy     =   ciVaporPolicy contractInfo
                ptDelta         =   ciPriceTierDelta contractInfo
                lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- shUtxos'])    
                                <>  Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- ptUtxos'])    
                                <>  Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- odUtxos'])      
                                <>  Constraints.unspentOutputs ownUtxos                                                 
                                <>  Constraints.otherScript hsVaporizeValidator                                     
                                <>  Constraints.typedValidatorLookups (hsVaporizeInstance contractInfo)
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData UpdateSh)    | (oref, _, _) <- shUtxos']   
                                <>  mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData UsePt)       | (oref, _, _) <- ptUtxos']  
                                <>  mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Vaporize)    | (oref, _, _) <- odUtxos']
                                <>  Constraints.mustPayToPubKey vaporizerPkh    (  
                                                                                    Ada.lovelaceValueOf minUtxoLovelace
                                                                                <>  Value.singleton originPolicy "HYPESKULL0001" 1
                                                                                )
                                <>  Constraints.mustPayToPubKey adminPkh (  Ada.lovelaceValueOf ( amt 
                                                                                                - minUtxoLovelace 
                                                                                                - vaporizerFee 
                                                                                                - ressDiscount
                                                                                                )
                                                                         <> Value.singleton originPolicy "HYPESKULLSRESURRECTION"   1
                                                                         <> Value.singleton vaporPolicy "HYPESKULLS_VT_AD_C"        1
                                                                         )
                                <>  mconcat [Constraints.mustPayToTheScript 
                                            (PtDatum $ amt + ptDelta) 
                                            (Value.singleton cs tn 1 <> Ada.lovelaceValueOf minUtxoLovelace) 
                                            | (_, _, AssetClass (cs, tn)) <- ptUtxos']
                                <>  mconcat [Constraints.mustPayToTheScript 
                                            (snd $ vpVTToken params)
                                            (Value.singleton cs tn 1 <> Ada.lovelaceValueOf minUtxoLovelace) 
                                            | (_, _, AssetClass (cs, tn)) <- shUtxos']

            logInfo @String $ "found sh utxos: " P.++ show (P.length shUtxos')
            logInfo @String $ "found pt utxos: " P.++ show (P.length ptUtxos')
            logInfo @String $ "found order utxos: " P.++ show (P.length odUtxos')
            ledgerTx <- submitTxConstraintsWith @HSVaporize lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Vaporization order added!"

refund :: (AsContractError e) => PubKeyHash -> Contract w s e ()
refund vaporizerPkh = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    odUtxos <- findScriptUtxos 0 $ ciVTAffix contractInfo
    case odUtxos of
        []          -> logInfo @String "No order utxos at script address"
        odUtxos'    -> do
            let vaporizerFee    =   ciVaporizerFee contractInfo
                originPolicy    =   ciOriginPolicy contractInfo
                vaporPolicy     =   ciVaporPolicy contractInfo
                lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- odUtxos'])      
                                <>  Constraints.unspentOutputs ownUtxos                                                 
                                <>  Constraints.otherScript hsVaporizeValidator                                     
                                <>  Constraints.typedValidatorLookups (hsVaporizeInstance contractInfo)
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Refund)    | (oref, _, _) <- odUtxos']
                                <>  Constraints.mustPayToPubKey vaporizerPkh    (  Ada.lovelaceValueOf (70_000_000 - vaporizerFee)
                                                                                <> Value.singleton originPolicy "HYPESKULL0001" 1
                                                                                <> Value.singleton originPolicy "HYPESKULLSRESURRECTION"   1
                                                                                <> Value.singleton vaporPolicy "HYPESKULLS_VT_M_C"         1
                                                                                )
            logInfo @String $ "found order utxos: " P.++ show (P.length odUtxos')
            ledgerTx <- submitTxConstraintsWith @HSVaporize lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Vaporization order refunded!"

deliver :: (AsContractError e) => DeliverParams -> Contract w s e ()
deliver params = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    shadowHSUtxos <- findScriptUtxos 13 (ciShadowHSAffix contractInfo)
    case shadowHSUtxos of
        []      -> logInfo @String "No utxos at script address"
        utxos   -> do
            let minUtxoLovelace =   ciMinUtxoLovelace contractInfo
                lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos])    <>
                                    Constraints.unspentOutputs ownUtxos                                             <>
                                    Constraints.otherScript hsVaporizeValidator                                     <>
                                    Constraints.typedValidatorLookups (hsVaporizeInstance contractInfo)                                  
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Deliver) | (oref, _, _) <- utxos]    <>
                                    Constraints.mustPayToPubKey (dpVaporizeePKH params) 
                                        (Value.singleton (ciVaporPolicy contractInfo) (fst $ dpVaporizedSkull params) 1                          <> 
                                        Ada.lovelaceValueOf (ciMinUtxoLovelace contractInfo))                                                               <>   
                                    mconcat [Constraints.mustPayToTheScript 
                                            (snd $ dpVaporizedSkull params)
                                            (Value.singleton cs tn' 1 <> Ada.lovelaceValueOf minUtxoLovelace) 
                                            | (_, _, AssetClass (cs, tn')) <- utxos]
            logInfo @String $ "found shadow utxos: " P.++ show (P.length utxos)
            ledgerTx <- submitTxConstraintsWith @HSVaporize lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Vaporized skull delivered!"


withdraw :: (AsContractError e) =>Contract w s e ()
withdraw = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    shadowHSUtxos <- findScriptUtxos 13 $ ciShadowHSAffix contractInfo
    ptUtxos <- findScriptUtxos 0 $ unTokenName $ ciPtTokenName contractInfo
    case (shadowHSUtxos, ptUtxos) of
        ([],[])  -> logInfo @String "No utxos at script address"
        (utxos, utxos')  -> do
            let lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos])    <>
                                    Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos'])   <>
                                    Constraints.unspentOutputs ownUtxos                                             <>
                                    Constraints.otherScript hsVaporizeValidator                                                        
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Withdraw) | (oref, _, _) <- utxos]   <>
                                    mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Withdraw) | (oref, _, _) <- utxos']
                                    
            logInfo @String $ "found shadow utxos: " P.++ show (P.length utxos) P.++ "vpt utxos: " P.++ show (P.length utxos')
            ledgerTx <- submitTxConstraintsWith @HSVaporize lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Script utxos withdrawn"



type HSVaporizeSchema =
            Endpoint "setup"        SetupParams
        .\/ Endpoint "vaporize"     VaporizeParams
        .\/ Endpoint "refund"       PubKeyHash
        .\/ Endpoint "deliver"      DeliverParams
        .\/ Endpoint "withdraw"     ()
        .\/ Endpoint "log"          ()

data SetupParams = SetupParams
    { spShadowUtxos      :: ![(TokenName, VaporizeDatum)]
    , spPriceTierUtxos   :: ![(TokenName, VaporizeDatum)]
    , spOrderUtxos       :: ![(Value.Value, VaporizeDatum)]
    } deriving (Generic, ToJSON, FromJSON)

data VaporizeParams = VaporizeParams
    { vpPrice           :: !Integer
    , vpVaporizerPkh    :: !PubKeyHash
    , vpVTToken         :: !(TokenName, VaporizeDatum)
    } deriving (Generic, ToJSON, FromJSON)

data DeliverParams = DeliverParams
    { dpVaporizeePKH    :: !PubKeyHash
    , dpVaporizedSkull  :: !(TokenName, VaporizeDatum)
    } deriving (Generic, ToJSON, FromJSON)

endpoints :: Contract () HSVaporizeSchema Text ()
endpoints = forever
    $ awaitPromise
    $           setup'      
    `select`    vaporize'   
    `select`    refund'   
    `select`    deliver'    
    `select`    withdraw'    
    `select`    log'
  where
    setup'      = endpoint @"setup"     $ \params -> setupContract params
    vaporize'   = endpoint @"vaporize"  $ \params -> vaporize params
    refund'     = endpoint @"refund"    $ \params -> refund params
    deliver'    = endpoint @"deliver"   $ \params -> deliver params
    withdraw'   = endpoint @"withdraw"  $ const withdraw
    log'        = endpoint @"log"       $ const logUtxos