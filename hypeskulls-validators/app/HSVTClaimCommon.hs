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


module HSVTClaimCommon
    ( ContractInfo (..)
    , contractInfo
    , (&&&)
    , (|||)
    , VTClaimDatum (..)
    , VTClaimAction (..)
    , AssetCount (..)
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
    , ciNonce               :: !BuiltinByteString
    , ciShadowHSPrefix      :: !BuiltinByteString
    , ciVTRandPrefix        :: !BuiltinByteString
    , ciVTPrefix            :: !BuiltinByteString
    , ciHSResurrectionTN    :: !TokenName
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

contractInfo :: ContractInfo
contractInfo = ContractInfo
    { ciAdminPKH            = "a096d51da85c3eaabe2718be7b59f51291979935ad77b8deb4622fa3"
    , ciPolicy              = "2f459a0a0872e299982d69e97f2affdb22919cafe1732de01ca4b36c"
    , ciMinUtxoLovelace     = 2_000_000
    , ciDefaultVTRandOwner  = ""
    , ciShadowHSPrefix      = "SH_"
    , ciVTRandPrefix        = "VTR"
    , ciVTPrefix            = "HYP"
    , ciNonce               = "testnonce"
    , ciHSResurrectionTN    = "HYPESKULLSRESURRECTION"
    }

data VTClaimDatum = ShadowHSDatum | VTRDatum PubKeyHash | VTDatum BuiltinByteString
    deriving (Generic, ToJSON, FromJSON)

newtype AssetCount = AssetCount (CurrencySymbol, TokenName, Integer)
instance Eq AssetCount where
    {-# INLINABLE (==) #-}
    AssetCount (acs, atn, an) == AssetCount (bcs, btn, bn) =    acs == bcs &&
                                                                atn == btn &&
                                                                an  == bn

data VTClaimAction = CommitSkull | CommitRandom | UseRandom | ClaimVT | Withdraw
    deriving (Generic, ToJSON, FromJSON)

{-# INLINABLE (|||) #-}
(|||) :: Bool -> Bool -> Bool
(|||) x y = if x then True else y

{-# INLINABLE (&&&) #-}
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then y else False

PlutusTx.makeIsDataIndexed  ''VTClaimDatum      [('ShadowHSDatum, 0), ('VTRDatum, 1), ('VTDatum, 2)]
PlutusTx.makeIsDataIndexed  ''VTClaimAction     [('CommitSkull, 0), ('CommitRandom, 1), ('UseRandom, 2), ('ClaimVT, 3), ('Withdraw, 4)]
PlutusTx.makeIsDataIndexed  ''ContractInfo      [('ContractInfo, 0)]

PlutusTx.makeLift           ''ContractInfo