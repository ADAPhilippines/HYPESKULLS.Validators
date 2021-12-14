{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import            Prelude
import            System.Environment

import            Cardano.Api
import            Plutus.V1.Ledger.Contexts
import qualified  Plutus.V1.Ledger.Api as Plutus
import qualified  PlutusTx.Prelude as P
import qualified  PlutusTx
import qualified  Data.ByteString.Short as SBS
import            HSVTClaimCommon
import            HSVaporizeCommon
import            HSVTClaimValidator
import            HSVaporizeValidator
import            Data.String (IsString(fromString))
import            PlutusTx.Builtins.Class (stringToBuiltinByteString)
import            Plutus.V1.Ledger.Crypto
import            Plutus.V1.Ledger.Value
import            Data.Aeson
import            Cardano.Api.Shelley ( fromPlutusData )
import            GHC.Generics

main :: IO ()
main = do
  writeHSVTClaimScript
  writeHSVaporizeScript


writeHSVTClaimScript :: IO()
writeHSVTClaimScript = 
  do
  putStrLn "HSVTClaim Datums:"
  print $ "ShadowHSDatum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData HSVTClaimCommon.ShadowHSDatum))
  print $ "VTRDatum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData $ VTRDatum "abcd"))
  print $ "VTDatum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData $ VTDatum "abcd"))
  
  putStrLn "HSVTClaim Actions:"
  print $ "CommitSkull: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData CommitSkull))
  print $ "CommitRandom: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData CommitRandom))
  print $ "UseRandom: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData UseRandom))
  print $ "ClaimVT: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData ClaimVT))
  print $ "Withdraw: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData HSVTClaimCommon.Withdraw))

  putStrLn $ "Writing output to: " ++ "./scripts/HSVTClaim.plutus"
  result <- writeFileTextEnvelope "./scripts/HSVTClaim.plutus" Nothing hsVTClaimSerialised
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

writeHSVaporizeScript :: IO()
writeHSVaporizeScript = 
  do
  putStrLn "HSVaporize Datums:"
  print $ "ShadowHSDatum value: " 
    <> encode (scriptDataToJson ScriptDataJsonDetailedSchema 
    $ fromPlutusData (PlutusTx.toData $ HSVaporizeCommon.ShadowHSDatum (VaporizeListDatum "abcd" ["MK_EE"] ["SP_C"])))
  print $ "PTDatum value: " 
    <> encode (scriptDataToJson ScriptDataJsonDetailedSchema 
    $ fromPlutusData (PlutusTx.toData $ PTDatum 70))
  
  putStrLn "HSVaporize Actions:"
  print $ "Vaporize: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData Vaporize))
  print $ "Deliver: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData Deliver))
  print $ "Withdraw: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData HSVaporizeCommon.Withdraw))

  putStrLn $ "Writing output to: " ++ "./scripts/HSVaporize.plutus"
  result <- writeFileTextEnvelope "./scripts/HSVaporize.plutus" Nothing hsVaporizeSerialised
  case result of
    Left err -> print $ displayError err
    Right () -> return ()