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
        nftUtxoVal tn   =   assetClassValue (AssetClass (ciPolicy contractInfo, tn)) 1 <> Ada.lovelaceValueOf minUtxoLovelace
        tx              =   mconcat[Constraints.mustPayToTheScript d (nftUtxoVal tn') | (tn', d) <- spShadowHSTNs params]  <>
                            mconcat[Constraints.mustPayToTheScript d (nftUtxoVal tn') | (tn', d) <- spPriceTierTNs params]
    ledgerTx <- submitTxConstraints (hsVaporizeInstance contractInfo) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "set up contract address"

findScriptUtxos :: (AsContractError e) => BuiltinByteString -> Contract w s e [(TxOutRef, ChainIndexTxOut, AssetClass)]
findScriptUtxos prefix = do
    utxos <- utxosAt hsVaporizeAddress
    return [ (oref, o, AssetClass (hypePolicyId, tn o)) | (oref, o) <- Map.toList utxos, tn o /= ""]
    where
        hypePolicyId = ciPolicy contractInfo
        utxoAssets utxo = [ (cs, tn', n) | (cs, tn', n) <- Value.flattenValue (txOutValue $ toTxOut utxo), cs == hypePolicyId]
        tn utxo =
            case utxoAssets utxo of
                [(_,tn',_)]    -> if isRightPrefix tn' then tn' else ""
                _               -> ""
        isRightPrefix tn' = prefix == P.sliceByteString 0 3 (unTokenName tn')

logUtxos :: (AsContractError e) => Contract w s e ()
logUtxos = do
    shadowHSUtxos <- findScriptUtxos "SH_"
    logInfo @String $ "found shadow utxos: " P.++ show (P.length shadowHSUtxos)


vaporize :: (AsContractError e) => Integer -> Contract w s e ()
vaporize amt = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    shadowHSUtxos <- findScriptUtxos "SH_"
    ptUtxos <- findScriptUtxos "VAP"
    case (shadowHSUtxos, ptUtxos) of
        ([],[])  -> logInfo @String "No utxos at script address"
        (utxos, utxos')  -> do
            let minUtxoLovelace =   ciMinUtxoLovelace contractInfo
                lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos])    <>
                                    Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos'])   <>
                                    Constraints.unspentOutputs ownUtxos                                             <>
                                    Constraints.otherScript hsVaporizeValidator                                     <>
                                    Constraints.typedValidatorLookups (hsVaporizeInstance contractInfo)                                  
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Vaporize) | (oref, _, _) <- utxos]   <>
                                    mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Vaporize) | (oref, _, _) <- utxos']  <>
                                    Constraints.mustPayToPubKey pkh (Value.singleton (ciPolicy contractInfo) "HYPESKULL0001" 1)                             <>
                                    Constraints.mustPayToPubKey (ciAdminPKH contractInfo) 
                                        (Value.singleton (ciPolicy contractInfo) "HYPESKULLS_VT_MK_EE" 1 <> Ada.lovelaceValueOf amt)                         <>   
                                    mconcat [Constraints.mustPayToTheScript 
                                            (PTDatum $ amt + 10) 
                                            (Value.singleton cs tn 1 <> Ada.lovelaceValueOf minUtxoLovelace) 
                                            | (_, _, AssetClass (cs, tn)) <- utxos']                                                                        <>
                                    mconcat [Constraints.mustPayToTheScript 
                                            (ShadowHSDatum (VaporizeListDatum pkh ["MK_EE","SP_C","ADR_E"])) 
                                            (Value.singleton cs tn 1 <> Ada.lovelaceValueOf minUtxoLovelace) 
                                            | (_, _, AssetClass (cs, tn)) <- utxos]
            logInfo @String $ "found shadow utxos: " P.++ show (P.length utxos) P.++ "vpt utxos: " P.++ show (P.length utxos')
            ledgerTx <- submitTxConstraintsWith @HSVaporize lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Vaporization order added!"



deliver :: (AsContractError e) => Contract w s e ()
deliver = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    shadowHSUtxos <- findScriptUtxos "SH_"
    case shadowHSUtxos of
        []      -> logInfo @String "No utxos at script address"
        utxos   -> do
            let minUtxoLovelace =   ciMinUtxoLovelace contractInfo
                vaporizeePKH    =   "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" :: PubKeyHash
                lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos])    <>
                                    Constraints.unspentOutputs ownUtxos                                             <>
                                    Constraints.otherScript hsVaporizeValidator                                     <>
                                    Constraints.typedValidatorLookups (hsVaporizeInstance contractInfo)                                  
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Deliver) | (oref, _, _) <- utxos]    <>
                                    Constraints.mustPayToPubKey vaporizeePKH 
                                        (Value.singleton (ciPolicy contractInfo) "HYPESKULL0001_MK_EE" 1                          <> 
                                        Ada.lovelaceValueOf (ciMinUtxoLovelace contractInfo))                                                               <>   
                                    mconcat [Constraints.mustPayToTheScript 
                                            (ShadowHSDatum (VaporizeListDatum vaporizeePKH ["SP_C","ADR_E"])) 
                                            (Value.singleton cs tn 1 <> Ada.lovelaceValueOf minUtxoLovelace) 
                                            | (_, _, AssetClass (cs, tn)) <- utxos]
            logInfo @String $ "found shadow utxos: " P.++ show (P.length utxos)
            ledgerTx <- submitTxConstraintsWith @HSVaporize lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Vaporized skull delivered!"


withdraw :: (AsContractError e) =>Contract w s e ()
withdraw = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    shadowHSUtxos <- findScriptUtxos "SH_"
    ptUtxos <- findScriptUtxos "VAP"
    case (shadowHSUtxos, ptUtxos) of
        ([],[])  -> logInfo @String "No utxos at script address"
        (utxos, utxos')  -> do
            let minUtxoLovelace =   ciMinUtxoLovelace contractInfo
                lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos])    <>
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
            Endpoint "setup"    SetupParams
        .\/ Endpoint "vaporize"  Integer
        .\/ Endpoint "deliver"  ()
        .\/ Endpoint "withdraw"  ()
        .\/ Endpoint "log"      ()

data SetupParams = SetupParams
    { spShadowHSTNs     :: ![(TokenName, VaporizeDatum)]
    , spPriceTierTNs    :: ![(TokenName, VaporizeDatum)] 
    } deriving (Generic, ToJSON, FromJSON)

endpoints :: Contract () HSVaporizeSchema Text ()
endpoints = forever
    $ awaitPromise
    $   setup'      `select` 
        vaporize'   `select` 
        deliver'    `select`  
        withdraw'    `select` 
        log'
  where
    setup'      = endpoint @"setup"     $ \params -> setupContract params
    vaporize'   = endpoint @"vaporize"  $ \params -> vaporize params
    deliver'    = endpoint @"deliver"   $ const deliver
    withdraw'   = endpoint @"withdraw"  $ const withdraw
    log'        = endpoint @"log"       $ const logUtxos