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

module Registrar where

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
import Utils


-- MINTING --

tryMint :: RegParam -> (TxOutRef, ChainIndexTxOut) -> Contract w RegSchema Text ()
tryMint p (oref, ci) = do
  case _ciTxOutDatum ci of
    Left _    -> Contract.logError @String "no datum found"
    Right dat -> do
      let valA = _ciTxOutValue ci
      case exactTuition valA of
        False -> Contract.logError @String "incorrect tuition amount"
        True  -> do
          let valM    = Value.singleton (curSymbol oref) regToken 1
              val     = valM <> valA
              utxo    = Map.fromList [(oref, ci)]
              lookups = Constraints.mintingPolicy (regPolicy oref) <>
                        Constraints.unspentOutputs utxo            <>
                        Constraints.otherScript (regValidator p)
              tx      = Constraints.mustMintValue valM                         <>
                        Constraints.mustSpendScriptOutput oref unitRedeemer    <>
                        Constraints.mustPayToOtherScript (regValHash p) dat val
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          Contract.logInfo @String $ printf "forged %s" (show val)

mint :: RegParam -> Contract w RegSchema Text ()
mint p = do
  utxos <- utxosAt $ (scrAddress p)
  let utxosList = Map.toList utxos
  Contract.logInfo @String $ if PlutusTx.Foldable.null utxosList
    then "no UTxO's at registration script"
    else "started minting of enrollment token(s)"

  -- For each UTxO in 'utxosList' try to mint a token.  (Note that the case of
  -- an empty list is handled correctly.)
  PlutusTx.Prelude.foldr (\u r -> tryMint p u >> r) (return ()) utxosList


-- DELIVERING TOKEN --

-- Delivers non-Ada tokens in given UTxO to the Student's wallet whose
-- PubKeyHash is in the datum, and transfers the tuition funds in said
-- UTxO to the Registrar's wallet
tryDeliverTk :: (TxOutRef, ChainIndexTxOut) -> Contract w RegSchema Text ()
tryDeliverTk (oref, ci) = do
  case _ciTxOutDatum ci of
    Left _    -> Contract.logError @String "no datum found"
    Right dat -> do
      let datpkh = fromBuiltinData @PubKeyHash $ getDatum dat
      case datpkh of
        Nothing  -> Contract.logError @String "can not recover PubKeyHash"
        Just pkh -> do
          Contract.logInfo @String $ "student's PKH: " ++ show pkh
          let valA = toValue minAdaTxOut
              valT = removeAda $ _ciTxOutValue ci
              val  = valA <> valT
              utxo    = Map.fromList [(oref, ci)]
              lookups = Constraints.unspentOutputs utxo
              tx      = Constraints.mustSpendScriptOutput oref unitRedeemer     <>
                        Constraints.mustPayToPubKey (PaymentPubKeyHash pkh) val
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          Contract.logInfo @String $ "delivered token to student's wallet"

            
deliverTks :: RegParam -> Contract w RegSchema Text ()
deliverTks p = do
  utxos <- utxosAt $ (scrAddress p)
  let utxosList = Map.toList utxos
      utxosTkList = PlutusTx.Prelude.filter (hasTokens . _ciTxOutValue . snd) utxosList
  Contract.logInfo @String $ if PlutusTx.Foldable.null utxosTkList
    then "no UTxO's with (non Ada) tokens at registration script"
    else "started delivering enrollment token(s) to the student(s) and transfering tuiton funds to Registrar"

  -- For each UTxO in 'utxosTkList' try to deliver the (non Ada) tokens to the
  -- students.  (Note that the case when 'utxosTkList' is the empty list is
  -- handled correctly.)
  PlutusTx.Prelude.foldr (\u r -> tryDeliverTk u >> r) (return ()) utxosTkList


-- LOG UTXO's --

data LogParams = LogParams
  { lpAddress :: !Ledger.Address
  } deriving (Generic, ToJSON, FromJSON)

type RegSchema = Endpoint "mint" ()
             .\/ Endpoint "deliverTks" ()
             .\/ Endpoint "logU" LogParams

logU :: AsContractError e => LogParams -> Contract w s e ()
logU lp = do
  utxos <- utxosAt $ lpAddress lp
  logInfo @String $ "UTxO's: " ++ (show . Map.toList $ utxos)

