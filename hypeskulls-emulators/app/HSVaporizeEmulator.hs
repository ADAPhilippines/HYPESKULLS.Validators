{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HSVaporizeEmulator
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

import              HSVaporizeOffchain
import              HSVaporizeCommon


vaporCS :: CurrencySymbol
vaporCS = HSVaporizeCommon.ciVaporPolicy HSVaporizeCommon.contractInfo

originCS :: CurrencySymbol
originCS = HSVaporizeCommon.ciOriginPolicy HSVaporizeCommon.contractInfo


wallet :: Integer -> Wallet
wallet = fromWalletNumber . WalletNumber

runEmulator :: Haskell.IO()
runEmulator = do
    runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left $ Map.fromList [(wallet 1, v1), (wallet 2, v2), (wallet 3, defLovelace)]) def def

    defLovelace :: Value
    defLovelace = Ada.lovelaceValueOf 50_000_000

    v1 :: Value
    v1 =    defLovelace                                           <>
            Value.singleton vaporCS "SH_HYPESKULL0001"        1   <>
            Value.singleton vaporCS "VAPOR_PT_001"            1   <>
            Value.singleton vaporCS "HYPESKULL0001_MK_EE"     1       

    v2 :: Value
    v2 = defLovelace                                              <>
            Value.singleton originCS "HYPESKULL0001"           1  <>
            Value.singleton vaporCS "HYPESKULLS_VT_MK_EE"      1

    myTrace :: EmulatorTrace ()
    myTrace = do
        h1 <- activateContractWallet (wallet 1) endpoints
        h2 <- activateContractWallet (wallet 2) endpoints
        void $ Emulator.waitNSlots 1
        -- callEndpoint @"setup" h1 SetupParams
        --     { spShadowHSTNs      =  [ ("SH_HYPESKULL0001",ShadowHSDatum (VaporizeListDatum "" ["SP_C","ADR_E"] []))
        --                             ]
        --     , spPriceTierTNs    =   [ ("VAPOR_PT_001", PTDatum 70)
        --                             ]
        --     }
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         h1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"withdraw"    h1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         h1 ()
        void $ Emulator.waitNSlots  1
