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
    , ciOriginPolicy        :: !CurrencySymbol
    , ciVaporPolicy         :: !CurrencySymbol
    , ciMinUtxoLovelace     :: !Integer
    , ciDefaultVRTOwner     :: !PubKeyHash
    , ciNonce               :: !BuiltinByteString
    , ciShadowHSAffix       :: !BuiltinByteString
    , ciVRTAffix            :: !BuiltinByteString
    , ciVTAffix             :: !BuiltinByteString
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

contractInfo :: ContractInfo
contractInfo = ContractInfo
    { ciAdminPKH            = "630341893fe6eab84475f06d34306d1c0a51d7258dedc81793f7b783"
    , ciOriginPolicy        = "2f459a0a0872e299982d69e97f2affdb22919cafe1732de01ca4b36c"
    , ciVaporPolicy         = "6f37a98bd0c9ced4e302ec2fb3a2f19ffba1b5c0c2bedee3dac30e56"
    , ciMinUtxoLovelace     = 3_000_000
    , ciDefaultVRTOwner     = ""
    , ciShadowHSAffix       = "_SH"
    , ciVRTAffix            = "HYPESKULLS_VRT_"
    , ciVTAffix             = "HYPESKULLS_VT_"
    , ciNonce               = "testnonce"
    }

data VTClaimDatum = ShadowHSDatum | VRTDatum PubKeyHash | VTDatum BuiltinByteString
    deriving (Generic, ToJSON, FromJSON)

newtype AssetCount = AssetCount (CurrencySymbol, TokenName, Integer)
instance Eq AssetCount where
    {-# INLINABLE (==) #-}
    AssetCount (acs, atn, an) == AssetCount (bcs, btn, bn) =    acs == bcs &&
                                                                atn == btn &&
                                                                an  == bn

data VTClaimAction = CommitSkull | CommitRandom | UseRandom | ProveOwner | ClaimVT | Withdraw
    deriving (Generic, ToJSON, FromJSON)

{-# INLINABLE (|||) #-}
(|||) :: Bool -> Bool -> Bool
(|||) x y = if x then True else y

{-# INLINABLE (&&&) #-}
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then y else False

PlutusTx.makeIsDataIndexed  ''VTClaimDatum      [('ShadowHSDatum, 0), ('VRTDatum, 1), ('VTDatum, 2)]
PlutusTx.makeIsDataIndexed  ''VTClaimAction     [('CommitSkull, 0), ('CommitRandom, 1), ('UseRandom, 2), ('ProveOwner, 3), ('ClaimVT, 4), ('Withdraw, 5)]
PlutusTx.makeIsDataIndexed  ''ContractInfo      [('ContractInfo, 0)]

PlutusTx.makeLift           ''ContractInfo