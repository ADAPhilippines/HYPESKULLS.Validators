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

import              HSVTClaimValidator
import              HSVTClaimCommon


setupContract :: (AsContractError e) => SetupParams -> Contract w s e ()
setupContract params = do
    let minUtxoLovelace =   ciMinUtxoLovelace contractInfo
        nftUtxoVal tn   =   assetClassValue (AssetClass (ciPolicy contractInfo, tn)) 1 <> Ada.lovelaceValueOf minUtxoLovelace
        tx              =   mconcat[Constraints.mustPayToTheScript d (nftUtxoVal tn') | (tn', d) <- spVRTs params]  <>
                            mconcat[Constraints.mustPayToTheScript d (nftUtxoVal tn') | (tn', d) <- spVTs params]   <>
                            Constraints.mustPayToTheScript ShadowHSDatum (nftUtxoVal (spShadowHSTN params))

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
    vrtUtxos <- findScriptUtxos "VRT"
    logInfo @String $ "found shadow utxos: " P.++ show (P.length shadowHSUtxos)
    logInfo @String $ " VRT utxos: " P.++ show (P.length vrtUtxos)


commit :: (AsContractError e) => Contract w s e ()
commit = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    shadowHSUtxos <- findScriptUtxos "SH_"
    vrtUtxos <- findScriptUtxos "VRT"
    case (shadowHSUtxos, vrtUtxos) of
        ([],[])  -> logInfo @String "No utxos at script address"
        (utxos, utxos')  -> do
            let newDatum        =   VRTDatum pkh
                minUtxoLovelace =   ciMinUtxoLovelace contractInfo
                lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos])    <>
                                    Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos'])   <>
                                    Constraints.unspentOutputs ownUtxos                                             <>
                                    Constraints.otherScript hsVTClaimValidator                                      <>
                                    Constraints.typedValidatorLookups (hsVTClaimInstance contractInfo)
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData CommitSkull) | (oref, _, _) <- utxos]        <>
                                    mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData CommitRandom)| (oref, _, _) <- utxos']       <>
                                    Constraints.mustPayToPubKey 
                                            (ciAdminPKH contractInfo) 
                                            (mconcat[   Value.singleton cs tn 1             <>
                                                        Ada.lovelaceValueOf minUtxoLovelace     | (_, _, AssetClass (cs, tn)) <- utxos])                                <>
                                    mconcat [Constraints.mustPayToTheScript 
                                            newDatum 
                                            (Value.singleton cs tn 1 <> Ada.lovelaceValueOf minUtxoLovelace) 
                                            | (_, _, AssetClass (cs, tn)) <- utxos']                                                                                <>                
                                    Constraints.mustPayToPubKey pkh (Value.singleton (ciPolicy contractInfo) "HYPESKULL0001" 1)                                     
            logInfo @String $ "found shadow utxos: " P.++ show (P.length utxos) P.++ " VRT utxos: " P.++ show (P.length utxos')
            ledgerTx <- submitTxConstraintsWith @HSVTClaim lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "skull commit successful"

claim :: (AsContractError e) => Contract w s e ()
claim = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    vrtUtxos <- findScriptUtxos "VRT"
    vtUtxos <- findScriptUtxos "HYP"
    case (vrtUtxos, vtUtxos) of
        ([],[])         -> logInfo @String "No valid utxos at script address"
        (_,[])          -> logInfo @String "No valid VRT utxos at script address"
        ([],_)          -> logInfo @String "No valid VT utxos at script address"
        (utxos, utxos') -> do
            let minUtxoLovelace =   ciMinUtxoLovelace contractInfo
                lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos'])   <>
                                    Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos])    <>
                                    Constraints.unspentOutputs ownUtxos                                             <>
                                    Constraints.otherScript hsVTClaimValidator
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimVT) | (oref, _, _) <- utxos']   <>
                                    mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData UseRandom) | (oref, _, _) <- utxos]  <>   
                                    Constraints.mustPayToPubKey 
                                        (ciAdminPKH contractInfo) 
                                        (mconcat[Value.singleton cs tn 1 <> Ada.lovelaceValueOf minUtxoLovelace | (_, _, AssetClass (cs, tn)) <- utxos])    <>
                                    Constraints.mustPayToPubKey 
                                        (ciAdminPKH contractInfo) 
                                        (mconcat[Ada.lovelaceValueOf minUtxoLovelace | (_, _, _) <- utxos'])
            logInfo @String $ "found VRT utxos: " P.++ show (P.length utxos)
            logInfo @String $ "found vt utxos: " P.++ show (P.length utxos')
            ledgerTx <- submitTxConstraintsWith @HSVTClaim lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Vapor Tokens Claimed"

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


withdraw :: (AsContractError e) => Contract w s e ()
withdraw = do
    shadowHSUtxos <- findScriptUtxos "SH_"
    vrtUtxos <- findScriptUtxos "VRT"
    vtUtxos <- findScriptUtxos "HYP"
    case (shadowHSUtxos, vrtUtxos, vtUtxos) of
        ([],[],[])  -> logInfo @String "No utxos at script address"
        (utxos, utxos', utxos'')  -> do
            let lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos])    <>
                                    Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos'])   <>
                                    Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos''])  <>
                                    Constraints.otherScript hsVTClaimValidator                                                               
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Withdraw) | (oref, _, _) <- utxos]   <>
                                    mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Withdraw) | (oref, _, _) <- utxos']  <>
                                    mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Withdraw) | (oref, _, _) <- utxos'']
            logInfo @String $ "found shadow utxos: " P.++ show (P.length utxos) 
            logInfo @String $ "VRT utxos: " P.++ show (P.length utxos')
            logInfo @String $ "vt utxos: " P.++ show (P.length utxos'')
            ledgerTx <- submitTxConstraintsWith @HSVTClaim lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "Script utxos withdrawn"

proveOwner :: (AsContractError e) => Contract w s e ()
proveOwner = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    ownUtxos <- utxosAt $ pubKeyHashAddress pkh
    vrtUtxos <- findScriptUtxos "VRT"
    case vrtUtxos of
        []      -> logInfo @String "No valid utxos at script address"
        utxos   -> do
            let minUtxoLovelace =   ciMinUtxoLovelace contractInfo
                lookups         =   Constraints.unspentOutputs (Map.fromList [(oref, o)| (oref, o, _) <- utxos])    <>
                                    Constraints.unspentOutputs ownUtxos                                             <>
                                    Constraints.otherScript hsVTClaimValidator                                      <>
                                    Constraints.typedValidatorLookups (hsVTClaimInstance contractInfo)
                tx              =   mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ProveOwner) | (oref, _, _) <- utxos]   <>
                                    mconcat [Constraints.mustPayToTheScript 
                                            (VRTDatum pkh) 
                                            (Value.singleton cs tn 1 <> Ada.lovelaceValueOf minUtxoLovelace) 
                                            | (_, _, AssetClass (cs, tn)) <- utxos]
            ledgerTx <- submitTxConstraintsWith @HSVTClaim lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "VRT Ownership Proven."


type HSVTClaimSchema =
            Endpoint "setup" SetupParams
        .\/ Endpoint "commit" ()
        .\/ Endpoint "claim" ()
        .\/ Endpoint "prove" ()
        .\/ Endpoint "withdraw" ()
        .\/ Endpoint "log" ()

data SetupParams = SetupParams
    { spShadowHSTN  :: !TokenName 
    , spVRTs        :: ![(TokenName, VTClaimDatum)]
    , spVTs         :: ![(TokenName, VTClaimDatum)]
    } deriving (Generic, ToJSON, FromJSON)

endpoints :: Contract () HSVTClaimSchema Text ()
endpoints = forever
    $ awaitPromise
    $   setup'      `select` 
        commit'     `select` 
        prove'      `select` 
        claim'      `select` 
        withdraw'   `select` 
        log'
  where
    setup'      = endpoint @"setup"             $ \params -> setupContract params
    commit'     = endpoint @"commit"            $ const commit
    claim'      = endpoint @"claim"             $ const claim
    prove'      = endpoint @"prove"             $ const proveOwner
    withdraw'   = endpoint @"withdraw"          $ const withdraw
    log'        = endpoint @"log"               $ const logUtxos