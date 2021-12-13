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
  -- args <- getArgs
  -- let nargs = length args
  -- let scriptname = "./scripts/" ++ if nargs > 0 then head args else  "result.plutus"
  putStrLn $ "Writing output to: " ++ "./scripts/HSVTClaim.plutus"
  writePlutusScript "./scripts/HSVTClaim.plutus" hsVTClaimSerialised
  putStrLn $ "Writing output to: " ++ "./scripts/HSVaporize.plutus"
  writePlutusScript "./scripts/HSVaporize.plutus" hsVaporizeSerialised


writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> IO ()
writePlutusScript filename scriptSerial =
  do
  let shadowHSDatum = ShadowHSDatum
      vtrDatum = VTRDatum "abcd"
      vtDatum = VTDatum "abcd"
  print $ "ShadowHSDatum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData shadowHSDatum))
  print $ "VTRDatum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData vtrDatum))
  print $ "VTDatum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData vtDatum))
  print $ "Redeemer value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData ()))
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()