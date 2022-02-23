{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HSVaporizeEmulator
    ( runEmulator
    , getPkh
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
import Wallet.Emulator.Wallet (walletPubKey)


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
    emCfg = EmulatorConfig (Left $ Map.fromList [(wallet 1, v1), (wallet 2, v2), (wallet 3,  defLovelace)]) def def

    defLovelace :: Value
    defLovelace = Ada.lovelaceValueOf 100_000_000

    v1 :: Value
    v1 =       defLovelace                                             
            <> Value.singleton vaporCS   "HYPESKULL0001_SH"        1   
            <> Value.singleton vaporCS   "HYPESKULLS_PT"           1   
            <> Value.singleton originCS  "HYPESKULL0001"           1   
            <> Value.singleton originCS  "HYPESKULLSRESURRECTION"  5  
            <> Value.singleton vaporCS   "HYPESKULLS_VT_M_C"       1 
            <> Value.singleton vaporCS   "HYPESKULLS_VT_AD_C"      1 
            <> Value.singleton vaporCS   "HYPESKULL0001_AD_C"       1       

    v2 :: Value
    v2 =       defLovelace                                         
            -- <> Value.singleton originCS "HYPESKULL0001"           1  
            -- <> Value.singleton vaporCS "HYPESKULLS_VT_MK_EE"      1

    myTrace :: EmulatorTrace ()
    myTrace = do
        h1 <- activateContractWallet (wallet 1) endpoints
        h2 <- activateContractWallet (wallet 2) endpoints
        h3 <- activateContractWallet (wallet 3) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"setup" h1 SetupParams
            { spShadowUtxos      =  [ ("HYPESKULL0001_SH",ShadowHsDatum (VaporizeListDatum "" 0 0))
                                    ]
            , spPriceTierUtxos   =  [ ("HYPESKULLS_PT", PtDatum 70_000_000)
                                    ]
            , spOrderUtxos       =  [ (mconcat  [ Ada.lovelaceValueOf 70_000_000
                                                , Value.singleton originCS   "HYPESKULL0001"          1 
                                                , Value.singleton originCS  "HYPESKULLSRESURRECTION"  1
                                                , Value.singleton vaporCS   "HYPESKULLS_VT_AD_C"       1
                                                ]
                                    , OrderDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd")
                                    ]
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         h1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"vaporize"    h3 VaporizeParams
            { vpPrice               = 70_000_000
            , vpVaporizerPkh        = "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd"
            , vpVTToken             = ("HYPESKULLS_VT_AD_C", HSVaporizeCommon.ShadowHsDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" 1 0))
            }
        void $ Emulator.waitNSlots  1
        -- callEndpoint @"refund"      h3 "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd"
        callEndpoint @"deliver"     h1 DeliverParams
            { dpVaporizeePKH        = "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd"
            , dpVaporizedSkull      = ("HYPESKULL0001_AD_C", HSVaporizeCommon.ShadowHsDatum (VaporizeListDatum "fabc30d46356151102cc57d427d338b8790b2244c1250159685400dd" 1 1))
            }
        void $ Emulator.waitNSlots  1
        callEndpoint @"withdraw"    h1 ()
        void $ Emulator.waitNSlots  1
        callEndpoint @"log"         h1 ()
        void $ Emulator.waitNSlots  1


getPkh :: Integer -> PubKeyHash
getPkh n = pubKeyHash $ walletPubKey $ wallet n