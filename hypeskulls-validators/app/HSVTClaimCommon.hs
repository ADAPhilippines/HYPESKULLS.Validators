{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}


module HSVTClaimCommon
    ( ContractInfo (..)
    , contractInfo
    , (&&&)
    , (|||)
    , VTClaimDatum (..)
    , VTDatum (..)
    , VTClaimAction (..)
    ) where

import              Data.Aeson (FromJSON, ToJSON)
import              GHC.Generics
import              Ledger
import qualified    PlutusTx
import              PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified    Prelude as Pr (Show (..), Eq)

data ContractInfo = ContractInfo
    { ciAdminPKH            :: !PubKeyHash
    , ciPolicy              :: !CurrencySymbol
    , ciMinUtxoLovelace     :: !Integer
    , ciDefaultVTRandOwner  :: !PubKeyHash
    , ciShadowHSPrefix      :: !BuiltinByteString
    , ciVTRandPrefix        :: !BuiltinByteString
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

contractInfo :: ContractInfo
contractInfo = ContractInfo
    { ciAdminPKH            = "6ad510fe5e2eff4f367475f01ab79dc4cd1f2600bda02ab270577637"
    , ciPolicy              = "2f459a0a0872e299982d69e97f2affdb22919cafe1732de01ca4b36c"
    , ciMinUtxoLovelace     = 1_500_000
    , ciDefaultVTRandOwner  = ""
    , ciShadowHSPrefix      = "SH_"
    , ciVTRandPrefix        = "VTR"
    }

data VTDatum = VTDatum 
    { vtdOwner      :: !PubKeyHash
    , vtdHash       :: !BuiltinByteString
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

data VTClaimDatum = CommitSkullDatum | CommitRandomDatum VTDatum | ClaimDatum VTDatum
    deriving (Generic, ToJSON, FromJSON)

instance Eq VTClaimDatum where
    {-# INLINABLE (==) #-}
    CommitSkullDatum == CommitSkullDatum = True

instance Eq (CurrencySymbol, TokenName, Integer) where
    {-# INLINABLE (==) #-}
    (acs, atn, an) == (bcs, btn, bn) =  acs == bcs &&
                                        atn == btn &&
                                        an  == bn

data VTClaimAction = CommitSkull | CommitRandom | Claim
    deriving (Generic, ToJSON, FromJSON)

{-# INLINABLE (|||) #-}
(|||) :: Bool -> Bool -> Bool
(|||) x y = if x then True else y

{-# INLINABLE (&&&) #-}
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then y else False

PlutusTx.makeIsDataIndexed  ''VTDatum           [('VTDatum, 0)]
PlutusTx.makeIsDataIndexed  ''VTClaimDatum      [('CommitSkullDatum, 0), ('CommitRandomDatum, 1), ('ClaimDatum, 2)]
PlutusTx.makeIsDataIndexed  ''VTClaimAction     [('CommitSkull, 0), ('CommitRandom, 1), ('Claim, 2)]
PlutusTx.makeIsDataIndexed  ''ContractInfo      [('ContractInfo, 0)]

PlutusTx.makeLift           ''ContractInfo