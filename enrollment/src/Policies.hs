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

module Policies where

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


-- Enrollment tuition
tuition :: Ada
tuition = lovelaceOf 13000000

-- Check whether a value corresponds to the tuition plus 'minAda'
exactTuition :: Value -> Bool
exactTuition v = (fromValue v) == tuition <> minAdaTxOut

-- Token name of the enrollment NFT
regToken :: TokenName
regToken = TokenName $ stringToBuiltinByteString "ENROLLED"

{-# INLINABLE mkRegPolicy #-}
mkRegPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkRegPolicy oref () ctx = traceIfFalse "validating UTxO not present" validatesUTxO   &&
                          traceIfFalse "wrong token amount" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validatesUTxO :: Bool
    validatesUTxO = any correctUTxO $ txInfoInputs info

    correctUTxO :: TxInInfo -> Bool
    correctUTxO i = (txInInfoOutRef i) == oref                     &&  -- the selected UTxO
                    isJust (txOutDatumHash $ txInInfoResolved i)   &&  -- has Datum
                    exactTuition (txOutValue $ txInInfoResolved i)     -- exact payment of tution

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == regToken && amt == 1
        _               -> False

regPolicy :: TxOutRef -> Scripts.MintingPolicy
regPolicy oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkRegPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol oref = scriptCurrencySymbol $ regPolicy oref

