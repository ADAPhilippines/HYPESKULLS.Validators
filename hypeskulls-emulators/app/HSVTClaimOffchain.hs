{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE NumericUnderscores     #-}


module HSVTClaimOffchain
    ( endpoints
    ) where

import qualified    PlutusTx
import              PlutusTx.Builtins
import qualified    PlutusTx.Prelude        as P
import              Plutus.Contract         as Contract
import              Ledger
import              Ledger.Constraints      as Constraints
import              Ledger.Value            as Value
import qualified    Ledger.Ada              as Ada
import              Prelude
import              Data.Map                as Map
import              Data.Text
import              Control.Monad

import              HSVTClaimValidator
import              HSVTClaimCommon
import Plutus.V1.Ledger.Api (BuiltinByteString)


setupContract :: (AsContractError e) => TokenName -> Contract w s e ()
setupContract tn = do
    let shadowHSValue   =   assetClassValue (AssetClass (ciPolicy contractInfo, TokenName $ "SH_" `appendByteString` unTokenName tn)) 1 <> Ada.lovelaceValueOf 1_500_000
        vtRandValue     =   assetClassValue (AssetClass (ciPolicy contractInfo, TokenName $ "VTR_" `appendByteString` unTokenName tn)) 1 <> Ada.lovelaceValueOf 1_500_000
        vtRandDatum     =   CommitRandomDatum VTDatum { vtdOwner = "", vtdHash = "abcd1234" }
        tx              =   Constraints.mustPayToTheScript vtRandDatum vtRandValue  <>
                            Constraints.mustPayToTheScript CommitSkullDatum shadowHSValue

    ledgerTx <- submitTxConstraints (hsVTClaimInstance contractInfo) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "set up contract address"

findScriptUtxos :: (AsContractError e) => BuiltinByteString -> Contract w s e [(TxOutRef, ChainIndexTxOut, AssetClass)]
findScriptUtxos prefix = do
    utxos <- utxosAt hsVTClaimAddress
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
    vtrUtxos <- findScriptUtxos "VTR"
    logInfo @String $ "found shadow utxos: " P.++ show (P.length shadowHSUtxos)
    logInfo @String $ " vtr utxos: " P.++ show (P.length vtrUtxos)


commit :: (AsContractError e) => Contract w s e ()
commit = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    shadowHSUtxos <- findScriptUtxos "SH_"
    vtrUtxos <- findScriptUtxos "VTR"
    case (shadowHSUtxos, vtrUtxos) of
        ([],[])  -> logInfo @String "No utxos at script address"
        (utxos, utxos')  -> do
            let newDatum  =   CommitRandomDatum VTDatum { vtdOwner = pkh, vtdHash = "abcd1234" }
                lookups     =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos]) <>
                                Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos']) <>
                                Constraints.unspentOutputs ownUtxos <>
                                Constraints.otherScript hsVTClaimValidator <>
                                Constraints.typedValidatorLookups (hsVTClaimInstance contractInfo)
                tx          =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData CommitSkull) | (oref, _, _) <- utxos]        <>
                                mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData CommitRandom) | (oref, _, _) <- utxos']      <>
                                Constraints.mustPayToPubKey pkh (   Value.singleton (ciPolicy contractInfo) "HYPESKULL0001" 1)                                  <>
                                Constraints.mustPayToPubKey 
                                    (ciAdminPKH contractInfo) 
                                    (mconcat[Value.singleton cs tn 1 <> Ada.lovelaceValueOf 1_500_000 | (_, _, AssetClass (cs, tn)) <- utxos])                  <>
                                mconcat [Constraints.mustPayToTheScript newDatum (Value.singleton cs tn 1 <> Ada.lovelaceValueOf 1_500_000) | (_, _, AssetClass (cs, tn)) <- utxos']
            logInfo @String $ "found shadow utxos: " P.++ show (P.length utxos) P.++ "vtr utxos: " P.++ show (P.length utxos')
            ledgerTx <- submitTxConstraintsWith @HSVTClaim lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "funds unlocked"

cheat :: (AsContractError e) => Contract w s e ()
cheat = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    utxos <- utxosAt hsVTClaimAddress
    case Map.toList utxos of
        []  -> logInfo @String "No utxos at script address"
        xs  -> do
            let lookups =   Constraints.unspentOutputs utxos    <>
                            Constraints.unspentOutputs ownUtxos <>
                            Constraints.otherScript hsVTClaimValidator
                tx      =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData CommitSkull) | (oref, _) <- xs]      <>
                            Constraints.mustPayToPubKey pkh (   Value.singleton (ciPolicy contractInfo) "HYPESKULL0001" 1                           <>
                                                                Value.singleton (ciPolicy contractInfo) "HYPESKULL0002" 1)                          <>
                            Constraints.mustPayToPubKey (ciAdminPKH contractInfo) ( Value.singleton (ciPolicy contractInfo) "SH_HYPESKULL0001" 1    <>
                                                                                    Value.singleton (ciPolicy contractInfo) "SH_HYPESKULL0002" 1    <>
                                                                                    Value.singleton (ciPolicy contractInfo) "SH_HYPESKULL0003" 1)
            logInfo @String $ "found script utxos: " P.++ show (P.length xs)
            ledgerTx <- submitTxConstraintsWith @HSVTClaim lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "funds pwned"


type HSSwapSchema =
            Endpoint "setup" TokenName
        .\/ Endpoint "commit" ()
        .\/ Endpoint "cheat" ()
        .\/ Endpoint "log" ()

endpoints :: Contract () HSSwapSchema Text ()
endpoints = forever
    $ awaitPromise
    $ setup' `select` commit' `select` cheat' `select` log'
  where
    setup' = endpoint @"setup" $ \params -> setupContract params
    commit' = endpoint @"commit" $ const commit
    cheat' = endpoint @"cheat" $ const cheat
    log' = endpoint @"log" $ const logUtxos