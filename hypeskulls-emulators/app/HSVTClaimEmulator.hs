{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HSVTClaimEmulator
    ( runEmulator
    , runE2EEmulator
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

import              HSVTClaimOffchain       as VTClaim
import              HSVaporizeOffchain      as Vaporize
import              HSVTClaimCommon
import              HSVaporizeCommon


sHSCS :: CurrencySymbol
sHSCS = HSVTClaimCommon.ciPolicy HSVTClaimCommon.contractInfo

nonce :: BuiltinByteString
nonce = ciNonce HSVTClaimCommon.contractInfo  

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
    v1 =    defLovelace                                             <>
            Value.singleton sHSCS "HYPESKULL0001_SH"        1       <>
            Value.singleton sHSCS "HYPESKULL0002_SH"        1       <>
            Value.singleton sHSCS "HYPESKULLS_VRT_0057"     1       <>
            Value.singleton sHSCS "HYPESKULLS_VRT_1072"     1       <>
            Value.singleton sHSCS "HYPESKULLS_VT_SP_EE"     1       <>
            Value.singleton sHSCS "HYPESKULLS_VT_MK_EE"     1       <>
            Value.singleton sHSCS "HYPESKULLS_VT_A_C"       1       <>
            Value.singleton sHSCS "HYPESKULLS_VT_Z_E"       1       <>
            Value.singleton sHSCS "HYPESKULLS_VT_NUGGETS"   1       <>
            Value.singleton sHSCS "HYPESKULL0001_MK_EE"     1       <>
            Value.singleton sHSCS "HYPESKULL0001_SP_EE"     1       <>
            Value.singleton sHSCS "HYPESKULLS_PT"           1  

    v2 :: Value
    v2 = defLovelace                                                <>
            Value.singleton sHSCS "HYPESKULL0001"           1       <>
            Value.singleton sHSCS "HYPESKULL0002"           1       <>
            Value.singleton sHSCS "HYPESKULLSRESURRECTION"  2       

    myTrace :: EmulatorTrace ()
    myTrace = do
        h1 <- activateContractWallet (wallet 1) VTClaim.endpoints
        h2 <- activateContractWallet (wallet 2) VTClaim.endpoints
        h3 <- activateContractWallet (wallet 3) VTClaim.endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"setup" h1 VTClaim.SetupParams
            { spShadowHSTN  = "HYPESKULL0001_SH"
            , spVRTs        =   [ ("HYPESKULLS_VRT_0057", VRTDatum (ciDefaultVRTOwner HSVTClaimCommon.contractInfo)) ]
            , spVTs         =   [ ("HYPESKULLS_VT_SP_EE", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0057" <> "_" <> nonce))
                                , ("HYPESKULLS_VT_MK_EE", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0057" <> "_" <> nonce))
                                , ("HYPESKULLS_VT_NUGGETS", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0057" <> "_" <> nonce))
                                ]
            }
        -- void $ Emulator.waitNSlots 1
        -- callEndpoint @"setup" h1 VTClaim.SetupParams
        --     { spShadowHSTN  = "HYPESKULL0002_SH"
        --     , spVRTs        =   [ ("HYPESKULLS_VRT_1072", VRTDatum (ciDefaultVRTOwner HSVTClaimCommon.contractInfo))]
        --     , spVTs         =   [ ("HYPESKULLS_VT_A_C", VTDatum $ sha2_256 ("HYPESKULLS_VRT_1072" <> "_" <> nonce))
        --                         , ("HYPESKULLS_VT_Z_E", VTDatum $ sha2_256 ("HYPESKULLS_VRT_1072" <> "_" <> nonce))
        --                         ]
        --     }
        void $ Emulator.waitNSlots 1
        callEndpoint @"log" h1 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"commit" h2 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"prove" h2 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"log" h1 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"claim" h2 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"log" h1 ()
        void $ Emulator.waitNSlots 1


runE2EEmulator :: Haskell.IO()
runE2EEmulator = do
    runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left $ Map.fromList [(wallet 1, v1), (wallet 2, v2), (wallet 3, v3)]) def def

    defLovelace :: Value
    defLovelace = Ada.lovelaceValueOf 20_000_000

    v1 :: Value
    v1 =    defLovelace                                             <>
            Value.singleton sHSCS "HYPESKULL0001_SH"        1       <>
            Value.singleton sHSCS "HYPESKULLS_VRT_0057"     1       <>
            Value.singleton sHSCS "HYPESKULLS_VRT_1072"     1       <>
            Value.singleton sHSCS "HYPESKULLS_VT_SP_EE"     1       <>
            Value.singleton sHSCS "HYPESKULLS_VT_MK_EE"     1       <>
            Value.singleton sHSCS "HYPESKULLS_VT_NUGGETS"   1       <>
            Value.singleton sHSCS "HYPESKULL0001_MK_EE"     1       <>
            Value.singleton sHSCS "HYPESKULL0001_SP_EE"     1       <>
            Value.singleton sHSCS "HYPESKULLS_PT"           1        

    v2 :: Value
    v2 =    Ada.lovelaceValueOf 500_000_000                         <>
            Value.singleton sHSCS "HYPESKULL0001"           1       <>
            Value.singleton sHSCS "HYPESKULLSRESURRECTION"  1       

    v3 :: Value
    v3 =    Ada.lovelaceValueOf 500_000_000                         <>
            Value.singleton sHSCS "HYPESKULL0001"           1       <>
            Value.singleton sHSCS "HYPESKULLS_VT_SP_EE"     1                  

    myTrace :: EmulatorTrace ()
    myTrace = do
        hVTClaim1   <- activateContractWallet (wallet 1) VTClaim.endpoints
        hVTClaim2   <- activateContractWallet (wallet 2) VTClaim.endpoints
        hVaporize1  <- activateContractWallet (wallet 1) Vaporize.endpoints
        hVaporize2  <- activateContractWallet (wallet 2) Vaporize.endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"setup" hVTClaim1 VTClaim.SetupParams
            { spShadowHSTN  = "HYPESKULL0001_SH"
            , spVRTs        =   [ ("HYPESKULLS_VRT_0057", VRTDatum (ciDefaultVRTOwner HSVTClaimCommon.contractInfo))]
            , spVTs         =   [ ("HYPESKULLS_VT_SP_EE", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0057" <> "_" <> nonce))
                                , ("HYPESKULLS_VT_MK_EE", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0057" <> "_" <> nonce))
                                , ("HYPESKULLS_VT_NUGGETS", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0057" <> "_" <> nonce))
                                ]
            }
        void $ Emulator.waitNSlots 1
        callEndpoint @"log"     hVTClaim1 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"commit"  hVTClaim2 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"log"     hVTClaim1 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"claim"   hVTClaim2 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"log"     hVTClaim1 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"setup"   hVaporize1 Vaporize.SetupParams
            { spShadowHSTNs     =   [ ("HYPESKULL0001_SH", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "" [] [])) ]
            , spPriceTierTNs    =   [ ("HYPESKULLS_PT", PTDatum 70) ]
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         hVaporize1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"vaporize"    hVaporize2 VaporizeParams
            { vpPrice               = 70
            , vpVTToken             = ("HYPESKULLS_VT_MK_EE", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" ["MK_EE"] []))
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"vaporize"    hVaporize2 VaporizeParams
            { vpPrice               = 80
            , vpVTToken             = ("HYPESKULLS_VT_SP_EE", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" ["SP_EE","MK_EE"] []))
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"deliver"     hVaporize1 DeliverParams
            { dpVaporizeePKH        = "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd"
            , dpVaporizedSkull      = ("HYPESKULL0001_MK_EE", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" ["SP_EE","MK_EE"] ["MK_EE"]))
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         hVaporize1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"deliver"     hVaporize1 DeliverParams
            { dpVaporizeePKH        = "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd"
            , dpVaporizedSkull      = ("HYPESKULL0001_SP_EE", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" ["SP_EE","MK_EE"] ["SP_EE","MK_EE"]))
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         hVaporize1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"withdraw"    hVaporize1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         hVaporize1 ()
        void $ Emulator.waitNSlots  1