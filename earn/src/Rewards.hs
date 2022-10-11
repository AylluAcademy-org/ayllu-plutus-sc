{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Rewards where

import           Control.Monad          (void)
import           Data.Text              (Text)
import           Data.Void              (Void)
import qualified Data.Map               as Map
import           Plutus.Contract
import           PlutusTx.Prelude       as TxPrelude hiding (Semigroup(..), unless)
import           Plutus.Script.Utils.Scripts (datumHash)
import           Ledger.Ada             as Ada
import           Ledger.Constraints
import qualified Ledger                 as L
import qualified Ledger.Tx              as Tx
import           Ledger.Value
import           Prelude                (Show(..), String, (<>))
import           Text.Printf            (printf)
import           Plutus.V2.Ledger.Api

import Validators
import Setup
import Utils
import Params


-- Teacher rewards student

-- Two simultaneous steps are involved: a) teacher sends "Teacher Token" to
-- student, and b) teacher locks the Ayllu rewards (with Datum) in Vault 2.

reward :: TeacherParams -> Contract w RewardsSchema Text ()
reward tp = do
  ownPKH       <- ownFirstPaymentPubKeyHash
  utxosOwn     <- ownUtxos
  utxosVault1  <- utxosAt $ vault1Address ownPKH
  utxosStudent <- utxosAt $ L.pubKeyHashAddress (tpStudentPKH tp) Nothing
  let orefVault1   = head $ Map.keys utxosVault1
      aylluCS      = curSymbolFT aylluTokenName $ tpAdminPKH tp
      valueVault1  = mconcat $ Tx._ciTxOutValue <$> Map.elems utxosVault1
      ayllusVault1 = valueOf valueVault1 aylluCS aylluTokenName
      valToVault1' = singleton aylluCS aylluTokenName $ ayllusVault1 - (tpAylluAmount tp)
      valToVault1  = valMinAda <> valToVault1'
      valToVault2' = singleton aylluCS aylluTokenName $ tpAylluAmount tp
      valToVault2  = valMinAda <> valToVault2'
  -- Begin reading of the enrollment NFT from the student's wallet.  In actuality
  -- this step will be achieved by just reading the Teacher's centralized ledger.
  let utxosTk     = Map.filter (hasTokenNamed enrollmentTokenName . Tx._ciTxOutValue) utxosStudent
      utxosTkList = Map.toList utxosTk
  logInfo @String $ printf "found %d UtxOs' with enrollment NFTs" (length utxosTkList)
  case utxosTkList of
    [] -> logInfo @String "no Enrollment NFT found"
    (_, ci) : _ -> do
      let valNFT = Tx._ciTxOutValue ci
          nftCS  = head . filter (/= Ada.adaSymbol) . symbols $ valNFT
      logInfo @String $ printf "NFT currency symbol: %s" (show $ unCurrencySymbol nftCS)
  -- End reading of the enrollment NFT
      let dat     = Datum $ toBuiltinData $ ValidationDatum { vdSymbol = nftCS }
          datHash = datumHash dat
          lookups = plutusV1OtherScript (vault1 ownPKH) <>
                    unspentOutputs utxosVault1
          tx      = mustSpendScriptOutput orefVault1 L.unitRedeemer <>
                    mustPayToOtherScript vault2Hash dat valToVault2 <>
                    mustPayToOtherScript (vault1Hash ownPKH) L.unitDatum valToVault1
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
      logInfo @String $ printf "sent %d Ayllus with datum to vault 2" (tpAylluAmount tp)
      let txOuts     = Tx.getCardanoTxOutRefs ledgerTx
          orefToStud = snd . head $ filter ((== Just datHash) . L.txOutDatumHash . fst) txOuts
          dat'       = Datum $ toBuiltinData $ LookupDatum { ldTxOutRef = orefToStud , ldAylluAmount = tpAylluAmount tp}
          teacherCS  = curSymbolFT teacherTokenName ownPKH
          valTeachTk = singleton teacherCS teacherTokenName 1
          val'       = valMinAda <> valTeachTk
          utxosOwnTk = Map.filter (hasTokensOf teacherCS teacherTokenName . Tx._ciTxOutValue) utxosOwn
      case Map.toList utxosOwnTk of
        []                 -> logInfo @String "no Teacher Tokens available"
        (orefOwnTk, _) : _ -> do
          let lookups' = unspentOutputs utxosOwnTk
              tx'      = mustSpendPubKeyOutput orefOwnTk                      <>
                         mustPayWithDatumToPubKey (tpStudentPKH tp) dat' val'
          ledgerTx' <- submitTxConstraintsWith @Void lookups' tx'
          void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx'
          logInfo @String $ "sent Teacher Token to student's wallet"


-- Student retrieves rewards from Vault2

-- The important feature is that the Enrollment NFT is used to validate that the
-- student has the privilage of retrieving the Ayllu rewards.  Note that the
-- student could be retrieving various rewards associated to different Teacher
-- tokens.

retrieve :: RetrieveParams -> Contract w RewardsSchema Text ()
retrieve rp = do
  ownPKH      <- ownFirstPaymentPubKeyHash
  utxosOwn    <- ownUtxos
  utxosVault2 <- utxosAt vault2Address
  let utxosNft = Map.filter (hasTokenNamed enrollmentTokenName . Tx._ciTxOutValue) utxosOwn
      orefsNft = Map.keys utxosNft
      aylluCS  = curSymbolFT aylluTokenName $ rpAdminPKH rp  
  case orefsNft of
    [] -> logInfo @String "no Enrollment NFT found"
    _  -> do
      let ciTTks = filter (hasTokenNamed teacherTokenName . Tx._ciTxOutValue) (Map.elems utxosOwn)
      case ciTTks of
        [] -> logInfo @String "no Teacher Token found"
        _  -> do
          let (lks, txs) = mconcat $ constraintsTTk aylluCS ownPKH utxosVault2 <$> ciTTks
          if null lks
            then logInfo @String "no rewards info on record"
            else do
              let lookups1      = plutusV1OtherScript vault2 <> unspentOutputs utxosNft
                  tx1           = (mconcat $ mustSpendPubKeyOutput <$> orefsNft)
                  (lookups, tx) = consolidate (lookups1, tx1) (lks, txs)
              ledgerTx <- submitTxConstraintsWith @Void lookups tx
              void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
              logInfo @String "retrieved Ayllu rewards"


-- Helper function 'constraintsTTk' allows to retrieve the reference to the UTxO
-- on Vault2 that stores the Ayllu reward; this is stored in the Datum
-- associated with the Teacher token.

constraintsTTk :: forall a i o. CurrencySymbol -> L.PaymentPubKeyHash ->
               Map.Map Tx.TxOutRef Tx.ChainIndexTxOut -> Tx.ChainIndexTxOut ->
               ([ScriptLookups a], TxConstraints i o)
constraintsTTk aylluCS' ownPKH' utxosVault2' ciTTk = 
  case Tx._ciTxOutPublicKeyDatum ciTTk of
    Nothing               -> ([], mempty)
    Just (_, Nothing)     -> ([], mempty)
    Just (_, Just datTTk) -> let
        lookupTTkM      = fromBuiltinData $ getDatum datTTk
        orefTTkM        = ldTxOutRef <$> lookupTTkM
        aylluAmountTTkM = ldAylluAmount <$> lookupTTkM
      in
        case (orefTTkM, aylluAmountTTkM) of
        (Nothing, _)                        -> ([], mempty)
        (_, Nothing)                        -> ([], mempty)
        (Just orefTTk, Just aylluAmountTTk) -> if aylluAmountTTk <= 0
          then ([], mempty)
          else let
            utxosTTk  = Map.filterWithKey (\o _ -> (o == orefTTk)) utxosVault2'
            valReward = singleton aylluCS' aylluTokenName aylluAmountTTk
            val       = valMinAda <> valReward
            lookups   = unspentOutputs utxosTTk
            tx        = mustSpendScriptOutput orefTTk L.unitRedeemer <>
                        mustPayToPubKey ownPKH' val
          in
            ([lookups], tx)

-- TODO:  Student must give the Teacher Token back in exchange of retrieving the Ayllu rewards.



