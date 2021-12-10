{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HSVTClaimEmulator
    ( runEmulator
    , hash
    , test
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
    emCfg = EmulatorConfig (Left $ Map.fromList [(wallet 1, v1), (wallet 2, v2), (wallet 3, defLovelace)]) def def

    defLovelace :: Value
    defLovelace = Ada.lovelaceValueOf 50_000_000

    v1 :: Value
    v1 =    defLovelace                                     <>
            Value.singleton sHSCS "SH_HYPESKULL0001" 1      <>
            Value.singleton sHSCS "SH_HYPESKULL0002" 1      <>
            Value.singleton sHSCS "SH_HYPESKULL0003" 1      <>
            Value.singleton sHSCS "VTR_HS_0001" 1           <>
            Value.singleton sHSCS "VTR_HS_0002" 1           <>
            Value.singleton sHSCS "HYPESKULLS_VT_SP_EE" 2   <>
            Value.singleton sHSCS "HYPESKULLS_VT_NUGGETS" 1

    v2 :: Value
    v2 = defLovelace                                        <>
            Value.singleton sHSCS "HYPESKULL0001" 1         <>
            Value.singleton sHSCS "HYPESKULL0002" 1         <>
            Value.singleton sHSCS "HYPESKULL0003" 1

    myTrace :: EmulatorTrace ()
    myTrace = do
        h1 <- activateContractWallet (wallet 1) endpoints
        h2 <- activateContractWallet (wallet 2) endpoints
        h3 <- activateContractWallet (wallet 3) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"setup" h1 SetupParams
            { spShadowHSTN  = "SH_HYPESKULL0001"
            , spVTRs        =   [ ("VTR_HS_0001", VTRDatum (ciDefaultVTRandOwner contractInfo))
                                , ("VTR_HS_0002", VTRDatum (ciDefaultVTRandOwner contractInfo))
                                ]
            , spVTs         =   [ ("HYPESKULLS_VT_SP_EE", VTDatum $ sha2_256 (ciNonce contractInfo `appendByteString` "VTR_HS_0001"))
                                , ("HYPESKULLS_VT_SP_EE", VTDatum $ sha2_256 (ciNonce contractInfo `appendByteString` "VTR_HS_0002"))
                                , ("HYPESKULLS_VT_NUGGETS", VTDatum $ sha2_256 (ciNonce contractInfo `appendByteString` "VTR_HS_0002"))
                                ]
            }
        void $ Emulator.waitNSlots 1
        callEndpoint @"log" h1 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"commit" h2 ()
        void $ Emulator.waitNSlots 3
        callEndpoint @"log" h1 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"claim" h2 ()
        void $ Emulator.waitNSlots 1
        callEndpoint @"log" h1 ()
        void $ Emulator.waitNSlots 1

hash :: BuiltinByteString -> BuiltinByteString
hash s = sha2_256 (ciNonce contractInfo `appendByteString` s)

getDiff :: Eq a => [a] -> [a] -> [a]
getDiff xs [] = xs
getDiff [] _ = []
getDiff (x:xs) ys =  if x `elem` ys then getDiff xs ys else x:xs


test :: [Integer] -> [Integer] -> [Integer]
test = getDiff
