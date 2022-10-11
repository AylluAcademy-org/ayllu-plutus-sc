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

module Setup where

import           Control.Monad       (void)
import           Data.Aeson          (ToJSON, FromJSON)
import           Data.Text           (Text)
import           Data.Void           (Void)
import qualified Data.Map            as Map
import           GHC.Generics        (Generic)
import           Plutus.Contract   
import           PlutusTx.Prelude    as TxPrelude hiding (Semigroup(..), unless)
import           Ledger.Ada          as Ada
import           Ledger.Constraints
import qualified Ledger              as L
import qualified Ledger.Tx           as Tx
import           Ledger.Value      
import           Prelude             (Show(..), String, (<>))
import           Text.Printf         (printf)

import Validators
import Params


-- SCHEMA

type RewardsSchema = Endpoint "mintAyllu" MintParams
                 .\/ Endpoint "mintTeacher" MintParams
                 .\/ Endpoint "mintNFT" NFTParams
                 .\/ Endpoint "adminTransfer" AdminParams
                 .\/ Endpoint "reward" TeacherParams
                 .\/ Endpoint "retrieve" RetrieveParams

data MintParams = MintParams
  { mpPKH    :: !L.PaymentPubKeyHash
  , mpAmount :: !Integer
  } deriving (Generic, ToJSON, FromJSON)

data NFTParams = NFTParams
  { npToken   :: !TokenName
  , npAddress :: !L.Address
  } deriving (Generic, FromJSON, ToJSON)

data AdminParams = AdminParams
  { apTeacherPKH  :: !L.PaymentPubKeyHash
  , apAylluAmount :: !Integer
  } deriving (Generic, FromJSON, ToJSON)

data TeacherParams = TeacherParams
  { tpStudentPKH  :: !L.PaymentPubKeyHash
  , tpAdminPKH    :: !L.PaymentPubKeyHash
  , tpAylluAmount :: !Integer
  } deriving (Generic, FromJSON, ToJSON)

data RetrieveParams = RetrieveParams
  { rpAdminPKH :: !L.PaymentPubKeyHash
  } deriving (Generic, FromJSON, ToJSON)


-- Minting (Ayllu & Teacher tokens)

mint :: TokenName -> MintParams -> Contract w RewardsSchema Text ()
mint tn mp = do
  let val    = singleton (curSymbolFT tn $ mpPKH mp) tn (mpAmount mp)
      lookups = plutusV1MintingPolicy . policyFT tn $ mpPKH mp
      tx      = mustMintValue val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
  logInfo @String $ printf "forged %s" (show val)

mintAyllu :: MintParams -> Contract w RewardsSchema Text ()
mintAyllu = mint aylluTokenName

mintTeacher :: MintParams -> Contract w RewardsSchema Text ()
mintTeacher = mint teacherTokenName


-- Mint NFTs

mintNFTUnsupervised :: NFTParams -> Contract w RewardsSchema Text ()
mintNFTUnsupervised np = do
  utxos <- utxosAt $ npAddress np
  case Map.keys utxos of
      []       -> logError @String "no utxo found"
      oref : _ -> do
          let tn      = npToken np
          let val     = singleton (curSymbolNFT oref tn) tn 1
              lookups = plutusV1MintingPolicy (policyNFT oref tn) <> unspentOutputs utxos
              tx      = mustMintValue val <> mustSpendPubKeyOutput oref
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
          logInfo @String $ printf "forged %s" (show val)

-- We use 'mintNFTUnsupervised' to setup the scenario where the student has
-- already her "Enrollment" NFT.  (The actual procedure for the student getting
-- this NFT is implemented elsewhere.)


-- Administrator delivers Ayllu's to Vault 1

adminTransfer :: AdminParams -> Contract w RewardsSchema Text ()
adminTransfer ap = do
  utxos  <- ownUtxos
  ownPKH <- ownFirstPaymentPubKeyHash
  let aylluCS = curSymbolFT aylluTokenName ownPKH
      val1    = toValue L.minAdaTxOut
      val2    = singleton aylluCS aylluTokenName $ apAylluAmount ap
      val     = val1 <> val2
      lookups = unspentOutputs utxos
      tx      = mustPayToOtherScript (vault1Hash $ apTeacherPKH ap) L.unitDatum val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
  logInfo @String $ printf "administrator transfered %d Ayllus to vault 1" (apAylluAmount ap)


