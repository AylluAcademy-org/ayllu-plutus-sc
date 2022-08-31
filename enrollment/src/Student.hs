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

module Student where

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

import Validators
import Policies


data PayParams = PayParams
    { ppStudent   :: !PaymentPubKeyHash
    , ppAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type PaySchema = Endpoint "pay" PayParams

pay :: AsContractError e => RegParam -> PayParams -> Contract w s e ()
pay p pp = do
    let dat = unPaymentPubKeyHash $ ppStudent pp
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ ppAmount pp
    ledgerTx <- submitTxConstraints (typedRegValidator p) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "payed registration fee of %d lovelace from %s"
        (ppAmount pp)
        (show $ ppStudent pp)
