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

module HSVaporizeCommon
    ( ContractInfo (..)
    , contractInfo
    , (&&&)
    , (|||)
    , VaporizeDatum (..)
    , VaporizeAction (..)
    , VaporizeOrderDatum (..)
    , VaporizeOrderListDatum (..)
    , AssetCount (..)
    ) where

import              Data.Aeson (FromJSON, ToJSON)
import              GHC.Generics
import              Ledger
import qualified    PlutusTx
import              PlutusTx.Builtins.Internal
import              PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified    Prelude as Pr (Show (..), Eq)

vaporTokenNames :: [BuiltinByteString]
vaporTokenNames = 
    [ "AD_C", "AD_E" , "AD_EE"
    , "AN_C", "AN_E", "AN_EE"
    , "G_C", "G_E", "G_EE"
    , "K_C", "K_E", "K_EE"
    , "M_C", "M_E", "M_EE"
    , "N_C", "N_E", " N_EE"
    , "P_C", "P_E", "P_EE"
    , "R_C", "R_E", "R_EE"
    , "V_C", "V_E", "V_EE"
    , "Z_C", "Z_E", "Z_EE"
    ]

data ContractInfo = ContractInfo
    { ciAdminPKH                :: !PubKeyHash
    , ciOriginPolicy            :: !CurrencySymbol
    , ciVaporPolicy             :: !CurrencySymbol
    , ciMinUtxoLovelace         :: !Integer
    , ciPriceTierDelta          :: !Integer
    , ciRessDiscount            :: !Integer
    , ciDefaultShadowHSOwner    :: !PubKeyHash
    , ciVTAffix                 :: !BuiltinByteString 
    , ciPtTokenName             :: !BuiltinByteString 
    , ciShadowHSAffix           :: !BuiltinByteString
    , ciRessTokenName           :: !TokenName  
    , ciVaporTokenNames         :: ![BuiltinByteString]
    , ciEmptyByteString         :: !BuiltinByteString
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

contractInfo :: ContractInfo
contractInfo = ContractInfo
    { ciAdminPKH                = "dbf64d0b70b89dab5872db86ff5d393bc548fcd3b8eba60610cae83c"
    , ciOriginPolicy            = "f3dfc1b6f369def06d1d576cfc98eb51e7e76ef1ecbb2f272c4f1621"
    , ciVaporPolicy             = "bf2c603d38ce68c6d875a097b5e6623fe0f5381d9171e06108e0aec9"
    , ciMinUtxoLovelace         = 1_500_000
    , ciPriceTierDelta          = 10
    , ciRessDiscount            = 20
    , ciDefaultShadowHSOwner    = ""
    , ciVTAffix                 = "HYPESKULLS_VT_"
    , ciPtTokenName             = "HYPESKULLS_PT"
    , ciShadowHSAffix           = "_SH"
    , ciRessTokenName           = "HYPESKULLSRESURRECTION"
    , ciVaporTokenNames         = vaporTokenNames
    , ciEmptyByteString         = ""
    }

data VaporizeOrderDatum = VaporizeOrderDatum
    { vodPkh            :: !PubKeyHash
    , vodAddr           :: !BuiltinByteString 
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

data VaporizeOrderListDatum = VaporizeOrderListDatum
    { vsdPkh            :: !PubKeyHash
    , vsdAddr           :: !BuiltinByteString 
    , vsdOrders         :: !Integer
    , vsdDelivered      :: !Integer
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

data VaporizeDatum = OrderDatum VaporizeOrderDatum | ShadowHsDatum VaporizeOrderListDatum | PtDatum Integer
    deriving (Generic, ToJSON, FromJSON)

data VaporizeAction = Vaporize | UpdateSh | UsePt | Deliver | Withdraw
    deriving (Generic, ToJSON, FromJSON)

{-# INLINABLE (|||) #-}
(|||) :: Bool -> Bool -> Bool
(|||) x y = if x then True else y

{-# INLINABLE (&&&) #-}
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then y else False

newtype AssetCount = AssetCount (CurrencySymbol, TokenName, Integer)
instance Eq AssetCount where
    {-# INLINABLE (==) #-}
    AssetCount (acs, atn, an) == AssetCount (bcs, btn, bn) =    acs == bcs &&
                                                                atn == btn &&
                                                                an  == bn

PlutusTx.makeIsDataIndexed  ''VaporizeDatum           [('OrderDatum, 0), ('ShadowHsDatum, 1), ('PtDatum, 2)]
PlutusTx.makeIsDataIndexed  ''VaporizeAction          [('Vaporize, 0), ('UpdateSh, 1), ('UsePt, 2), ('Deliver, 3), ('Withdraw, 4)]
PlutusTx.makeIsDataIndexed  ''VaporizeOrderDatum        [('VaporizeOrderDatum, 0)]
PlutusTx.makeIsDataIndexed  ''VaporizeOrderListDatum    [('VaporizeOrderListDatum, 0)]
PlutusTx.makeIsDataIndexed  ''ContractInfo              [('ContractInfo, 0)]

PlutusTx.makeLift           ''ContractInfo