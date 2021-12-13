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
    , VaporizeListDatum (..)
    , AssetCount (..)
    ) where

import              Data.Aeson (FromJSON, ToJSON)
import              GHC.Generics
import              Ledger
import qualified    PlutusTx
import              PlutusTx.Builtins.Internal
import              PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified    Prelude as Pr (Show (..), Eq)

data ContractInfo = ContractInfo
    { ciAdminPKH                :: !PubKeyHash
    , ciPolicy                  :: !CurrencySymbol
    , ciMinUtxoLovelace         :: !Integer
    , ciPriceTierDelta          :: !Integer
    , ciDefaultShadowHSOwner    :: !PubKeyHash
    , ciVaporTokenName          :: !BuiltinByteString 
    , ciPTTokenPrefix           :: !BuiltinByteString 
    , ciShadowHSPrefix      :: !BuiltinByteString
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

contractInfo :: ContractInfo
contractInfo = ContractInfo
    { ciAdminPKH                = "a096d51da85c3eaabe2718be7b59f51291979935ad77b8deb4622fa3"
    , ciPolicy                  = "2f459a0a0872e299982d69e97f2affdb22919cafe1732de01ca4b36c"
    , ciMinUtxoLovelace         = 2_000_000
    , ciPriceTierDelta          = 10
    , ciDefaultShadowHSOwner    = ""
    , ciVaporTokenName          = "HYPESKULLS_VT_"
    , ciPTTokenPrefix           = "VAP"
    , ciShadowHSPrefix          = "SH_"
    }

data VaporizeListDatum = VaporizeListDatum
    { vldPKH    :: !PubKeyHash
    , vldList   :: ![BuiltinByteString]
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

data VaporizeDatum = ShadowHSDatum VaporizeListDatum | PTDatum Integer
    deriving (Generic, ToJSON, FromJSON)

newtype AssetCount = AssetCount (CurrencySymbol, TokenName, Integer)
instance Eq AssetCount where
    {-# INLINABLE (==) #-}
    AssetCount (acs, atn, an) == AssetCount (bcs, btn, bn) =    acs == bcs &&
                                                                atn == btn &&
                                                                an  == bn

data VaporizeAction = Vaporize | Deliver | Withdraw
    deriving (Generic, ToJSON, FromJSON)

{-# INLINABLE (|||) #-}
(|||) :: Bool -> Bool -> Bool
(|||) x y = if x then True else y

{-# INLINABLE (&&&) #-}
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then y else False


PlutusTx.makeIsDataIndexed  ''VaporizeDatum     [('ShadowHSDatum, 0), ('PTDatum, 1)]
PlutusTx.makeIsDataIndexed  ''VaporizeAction    [('Vaporize, 0), ('Deliver, 1), ('Withdraw, 2)]
PlutusTx.makeIsDataIndexed  ''VaporizeListDatum [('VaporizeListDatum, 0)]
PlutusTx.makeIsDataIndexed  ''ContractInfo      [('ContractInfo, 0)]

PlutusTx.makeLift           ''ContractInfo