{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Validators where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.V1.Ledger.Api   (fromBuiltinData)
import           PlutusTx               (Data (..))
import qualified PlutusTx               
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           PlutusTx.Builtins.Class
import           PlutusTx.Foldable      (null)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     (TxConstraints)
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Ada             as Ada
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Semigroup (..), Show (..), String, last, undefined)
import           Text.Printf            (printf)
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet


data RegParam = RegParam
  { registrarPKH :: PaymentPubKeyHash } deriving Show

PlutusTx.makeLift ''RegParam

{-# INLINABLE mkRegValidator #-}
mkRegValidator :: RegParam -> PubKeyHash -> () -> ScriptContext -> Bool
mkRegValidator p _ _ ctx = traceIfFalse "consumer not authorized" signedByRegistrar
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByRegistrar :: Bool
    signedByRegistrar = txSignedBy info $ unPaymentPubKeyHash $ registrarPKH p

data RegInit
instance Scripts.ValidatorTypes RegInit where
    type instance DatumType RegInit = PubKeyHash
    type instance RedeemerType RegInit = ()

typedRegValidator :: RegParam -> Scripts.TypedValidator RegInit
typedRegValidator p = Scripts.mkTypedValidator @RegInit
    ($$(PlutusTx.compile [|| mkRegValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

regValidator :: RegParam -> Validator
regValidator = Scripts.validatorScript . typedRegValidator

regValHash :: RegParam -> Ledger.ValidatorHash
regValHash = Scripts.validatorHash . typedRegValidator

scrAddress :: RegParam -> Ledger.Address
scrAddress = scriptAddress . regValidator
