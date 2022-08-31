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

module Trace where

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
import Registrar
import Student


-- ENDPOINTS --

endpoints1 :: RegParam -> Contract () RegSchema Text ()
endpoints1 p = awaitPromise  (mint' `select` deliverTks' `select` logU') >> (endpoints1 p)
  where
    mint'       = endpoint @"mint" $ const (mint p)
    deliverTks' = endpoint @"deliverTks" $ const (deliverTks p)
    logU'       = endpoint @"logU" $ logU

endpoints2 :: RegParam -> Contract () PaySchema Text ()
endpoints2 p = awaitPromise pay' >> (endpoints2 p)
  where
    pay' = endpoint @"pay" (pay p)


-- TRACE EMULATOR --

test :: IO ()
test = runEmulatorTraceIO $ do
  let w1   = knownWallet 1 -- Role: the Registrar
      w2   = knownWallet 2 -- Role: Student1
      w3   = knownWallet 3 -- Role: Student2
      pkh1 = mockWalletPaymentPubKeyHash w1
      pkh2 = mockWalletPaymentPubKeyHash w2
      pkh3 = mockWalletPaymentPubKeyHash w3
      p    = RegParam { registrarPKH = pkh1 }
  h1 <- activateContractWallet w1 (endpoints1 p)
  h2 <- activateContractWallet w2 (endpoints2 p)
  h3 <- activateContractWallet w3 (endpoints2 p)

  -- student 1 pays tuition
  callEndpoint @"pay" h2 $ PayParams
    { ppStudent = pkh2
    , ppAmount  = 15000000
    }
  void $ Emulator.waitNSlots 1

  -- student 2 pays tuition
  callEndpoint @"pay" h3 $ PayParams
    { ppStudent = pkh3
    , ppAmount  = 15000000
    }
  void $ Emulator.waitNSlots 1

  -- log of utxos at script address
  callEndpoint @"logU" h1 $ LogParams
    { lpAddress = scrAddress p }
  void $ Emulator.waitNSlots 1

  -- minting of enrollment NFTs
  callEndpoint @"mint" h1 $ ()
  void $ Emulator.waitNSlots 2

  -- log of utxos at script address
  callEndpoint @"logU" h1 $ LogParams
    { lpAddress = scrAddress p }
  void $ Emulator.waitNSlots 1

  -- deliver enrollment NFTs to students' wallets, and transfer tuition funds to
  -- the Registrar's wallet
  callEndpoint @"deliverTks" h1 $ ()
  void $ Emulator.waitNSlots 1
  
