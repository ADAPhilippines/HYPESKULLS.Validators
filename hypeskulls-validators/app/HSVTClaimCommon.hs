{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module HSVTClaimCommon
    ( ContractInfo (..)
    , contractInfo
    , (&&&)
    , (|||)
    ) where

import              Data.Aeson (FromJSON, ToJSON)
import              GHC.Generics
import              Ledger
import qualified    PlutusTx
import              PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified    PlutusTx.Prelude as P
import qualified    Prelude as Pr (Show (..), Eq)

data ContractInfo = ContractInfo
    { ciAdminPKH        :: !PubKeyHash
    } deriving (Pr.Show, Pr.Eq, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 0)]
PlutusTx.makeLift ''ContractInfo

contractInfo :: ContractInfo
contractInfo = ContractInfo
    { ciAdminPKH        = "6ad510fe5e2eff4f367475f01ab79dc4cd1f2600bda02ab270577637"
    }

{-# INLINABLE (|||) #-}
(|||) :: Bool -> Bool -> Bool
(|||) x y = if x then True else y

{-# INLINABLE (&&&) #-}
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then y else False