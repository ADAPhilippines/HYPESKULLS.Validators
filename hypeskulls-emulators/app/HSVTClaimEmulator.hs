{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HSVTClaimEmulator
    ( runEmulator
    ) where

import              PlutusTx.Prelude        as Plutus
import              Plutus.Trace.Emulator   as Emulator
import              Wallet.Emulator.Wallet  as Wallet
import              Ledger
import              Ledger.Value            as Value
import qualified    Ledger.Ada              as Ada

import qualified    Prelude                 as Haskell
import              Data.Default            (def)
import qualified    Data.Map                as Map
import              Control.Monad           (void)

import              HSVTClaimOffchain
import              HSVTClaimCommon


sHSCS :: CurrencySymbol
sHSCS = ciPolicy contractInfo


wallet :: Integer -> Wallet
wallet = fromWalletNumber . WalletNumber

runEmulator :: Haskell.IO()
runEmulator = do
    runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left $ Map.fromList [(wallet 1, v1), (wallet 2, v2)]) def def

    defLovelace :: Value 
    defLovelace = Ada.lovelaceValueOf 15_000_000

    v1 :: Value
    v1 =    defLovelace                                <>
            Value.singleton sHSCS "SH_HYPESKULL0001" 1 <>
            Value.singleton sHSCS "SH_HYPESKULL0002" 1 <>
            Value.singleton sHSCS "SH_HYPESKULL0003" 1 <>
            Value.singleton sHSCS "VTR_HYPESKULL0001" 1 <>
            Value.singleton sHSCS "VTR_HYPESKULL0002" 1 

    v2 :: Value
    v2 = defLovelace                                    <>
            Value.singleton sHSCS "HYPESKULL0001" 1     <>
            Value.singleton sHSCS "HYPESKULL0002" 1     <>
            Value.singleton sHSCS "HYPESKULL0003" 1 

    myTrace :: EmulatorTrace ()
    myTrace = do
        h1 <- activateContractWallet (wallet 1) endpoints
        h2 <- activateContractWallet (wallet 2) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"setup" h1 "HYPESKULL0001"
        void $ Emulator.waitNSlots 1
        callEndpoint @"setup" h1 "HYPESKULL0002"
        void $ Emulator.waitNSlots 1
        callEndpoint @"log" h1 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"commit" h2 ()
        void $ Emulator.waitNSlots 3
        callEndpoint @"log" h1 ()
        void $ Emulator.waitNSlots 1
