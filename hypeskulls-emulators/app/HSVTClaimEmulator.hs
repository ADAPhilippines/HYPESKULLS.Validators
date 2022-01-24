{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module HSVTClaimEmulator
    ( runEmulator
    , runE2EEmulator
    , toBinary
    , testVal
    , test
    , vaporTokenNames
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
import qualified    GHC.OldList as L (findIndex) 


vaporCS :: CurrencySymbol
vaporCS = HSVTClaimCommon.ciVaporPolicy HSVTClaimCommon.contractInfo

originCS :: CurrencySymbol
originCS = HSVTClaimCommon.ciOriginPolicy HSVTClaimCommon.contractInfo

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
            Value.singleton vaporCS "HYPESKULL0001_SH"        1       <>
            Value.singleton vaporCS "HYPESKULL0002_SH"        1       <>
            Value.singleton vaporCS "HYPESKULLS_VRT_0001"     1       <>
            Value.singleton vaporCS "HYPESKULLS_VRT_1072"     1       <>
            Value.singleton vaporCS "HYPESKULLS_VT_SP_EE"     1       <>
            Value.singleton vaporCS "HYPESKULLS_VT_MK_EE"     1       <>
            Value.singleton vaporCS "HYPESKULLS_VT_A_C"       1       <>
            Value.singleton vaporCS "HYPESKULLS_VT_Z_E"       1       <>
            Value.singleton vaporCS "HYPESKULLS_VT_NUGGETS"   1       <>
            Value.singleton vaporCS "HYPESKULL0001_MK_EE"     1       <>
            Value.singleton vaporCS "HYPESKULL0001_SP_EE"     1       <>
            Value.singleton vaporCS "HYPESKULLS_PT"           1  

    v2 :: Value
    v2 = defLovelace                                                <>
            Value.singleton originCS "HYPESKULL0001"           1       <>
            Value.singleton originCS "HYPESKULL0002"           1       <>
            Value.singleton originCS "HYPESKULLSRESURRECTION"  2       

    myTrace :: EmulatorTrace ()
    myTrace = do
        h1 <- activateContractWallet (wallet 1) VTClaim.endpoints
        h2 <- activateContractWallet (wallet 2) VTClaim.endpoints
        h3 <- activateContractWallet (wallet 3) VTClaim.endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"setup" h1 VTClaim.SetupParams
            { spShadowHSTN  = "HYPESKULL0001_SH"
            , spVRTs        =   [ ("HYPESKULLS_VRT_0001", VRTDatum (ciDefaultVRTOwner HSVTClaimCommon.contractInfo)) ]
            , spVTs         =   [ ("HYPESKULLS_VT_SP_EE", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0001" <> "_" <> nonce))
                                , ("HYPESKULLS_VT_MK_EE", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0001" <> "_" <> nonce))
                                , ("HYPESKULLS_VT_NUGGETS", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0001" <> "_" <> nonce))
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
            Value.singleton vaporCS "HYPESKULL0001_SH"        2     <>
            Value.singleton vaporCS "HYPESKULLS_VRT_0830"     1     <>
            Value.singleton vaporCS "HYPESKULLS_VRT_1072"     1     <>
            Value.singleton vaporCS "HYPESKULLS_VT_AD_EE"     1     <>
            Value.singleton vaporCS "HYPESKULLS_VT_M_C"       1     <>
            Value.singleton vaporCS "HYPESKULLS_VT_NUGGETS"   1     <>
            Value.singleton vaporCS "HYPESKULL0001_M_C"       1     <>
            Value.singleton vaporCS "HYPESKULL0001_AD_EE"     1     <>
            Value.singleton vaporCS "HYPESKULL0001_AD_C"      1     <>
            Value.singleton vaporCS "HYPESKULLS_PT"           1        

    v2 :: Value
    v2 =    Ada.lovelaceValueOf 500_000_000                         <>
            Value.singleton originCS "HYPESKULL0001"           1    <>
            Value.singleton originCS "HYPESKULLSRESURRECTION"  1    <>
            Value.singleton vaporCS "HYPESKULLS_VT_M_C"        1       

    v3 :: Value
    v3 =    Ada.lovelaceValueOf 500_000_000                         <>
            Value.singleton originCS "HYPESKULL0001"           1    <>
            Value.singleton vaporCS "HYPESKULLS_VT_SP_EE"      1                  

    myTrace :: EmulatorTrace ()
    myTrace = do
        hVTClaim1   <- activateContractWallet (wallet 1) VTClaim.endpoints
        hVTClaim2   <- activateContractWallet (wallet 2) VTClaim.endpoints
        hVaporize1  <- activateContractWallet (wallet 1) Vaporize.endpoints
        hVaporize2  <- activateContractWallet (wallet 2) Vaporize.endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"setup" hVTClaim1 VTClaim.SetupParams
            { spShadowHSTN  = "HYPESKULL0001_SH"
            , spVRTs        =   [ ("HYPESKULLS_VRT_0830", VRTDatum (ciDefaultVRTOwner HSVTClaimCommon.contractInfo))]
            , spVTs         =   [ ("HYPESKULLS_VT_AD_EE", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0830" <> "_" <> nonce))
                                , ("HYPESKULLS_VT_M_C", VTDatum $ sha2_256 ("HYPESKULLS_VRT_0830" <> "_" <> nonce))
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
            { spShadowHSTNs     =   [ ("HYPESKULL0001_SH", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "" 0 0)) ]
            , spPriceTierTNs    =   [ ("HYPESKULLS_PT", PTDatum 70) ]
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         hVaporize1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"vaporize"    hVaporize2 VaporizeParams
            { vpPrice               = 70
            , vpVTToken             = ("HYPESKULLS_VT_M_C", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" 4096 0))
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"vaporize"    hVaporize2 VaporizeParams
            { vpPrice               = 80
            , vpVTToken             = ("HYPESKULLS_VT_AD_EE", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" 4100 0))
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"deliver"     hVaporize1 DeliverParams
            { dpVaporizeePKH        = "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd"
            , dpVaporizedSkull      = ("HYPESKULL0001_M_C", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" 4100 4096))
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         hVaporize1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"deliver"     hVaporize1 DeliverParams
            { dpVaporizeePKH        = "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd"
            , dpVaporizedSkull      = ("HYPESKULL0001_AD_C", HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" 4100 4097))
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         hVaporize1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"withdraw"    hVaporize1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         hVaporize1 ()
        void $ Emulator.waitNSlots  1

toBinary :: Haskell.Int -> [Haskell.Int]
toBinary n = if n Haskell.== 0 then [] else Haskell.mod n 2 : toBinary (Haskell.div n 2)

testVal :: Haskell.Int -> Haskell.Int -> Haskell.String
testVal old new = 
    case isUnique && isPowOf2 of
        False -> ""
        True -> vaporTokenNames Haskell.!! vtIdx
        where 
            !vtIdx      = log2 diff
            !diff       = new Haskell.- old
            !oldBin     = toBinary old
            !isPowOf2   = 2 Haskell.^ vtIdx Haskell.== diff
            !isUnique   = (Haskell.length oldBin Haskell.<= vtIdx) || 0 Haskell.== oldBin Haskell.!! vtIdx

getVTName :: Haskell.Int -> Haskell.Int -> Haskell.String
getVTName old new = vaporTokenNames Haskell.!! log2 (new Haskell.- old)

log2 :: Haskell.Int -> Haskell.Int
log2 n = if n Haskell.== 1 then 0 else 1 Haskell.+ log2 (n `Haskell.div` 2) 

test = toBinary


vaporTokenNames :: [Haskell.String]
vaporTokenNames = 
    [ "AD_C"
    , "AD_E" 
    , "AD_EE"
    , "AN_C"
    , "AN_E"
    , "AN_EE"
    , "G_C"
    , "G_E"
    , "G_EE"
    , "K_C"
    , "K_E"
    , "K_EE"
    , "M_C"
    , "M_E"
    , "M_EE"
    , "N_C"
    , "N_E"
    , "N_EE"
    , "P_C"
    , "P_E"
    , "P_EE"
    , "R_C"
    , "R_E"
    , "R_EE"
    , "V_C"
    , "V_E"
    , "V_EE"
    , "Z_C"
    , "Z_E"
    , "Z_EE"
    ]