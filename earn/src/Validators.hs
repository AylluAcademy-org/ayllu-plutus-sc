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

module Validators where

import qualified PlutusTx
import           PlutusTx.Prelude       as TxPrelude hiding (Semigroup(..), unless)
import qualified Ledger                 as L
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Tx              as Tx
import           Ledger.Value


-- Typed datum

data ValidationDatum = ValidationDatum { vdSymbol :: !CurrencySymbol }

PlutusTx.unstableMakeIsData ''ValidationDatum
PlutusTx.makeLift ''ValidationDatum

data LookupDatum = LookupDatum
  { ldTxOutRef    :: !L.TxOutRef
  , ldAylluAmount :: !Integer
  }
PlutusTx.unstableMakeIsData ''LookupDatum
PlutusTx.makeLift ''LookupDatum

-- Fungible Tokens

{-# INLINABLE mkPolicyFT #-}
mkPolicyFT :: TokenName -> L.PaymentPubKeyHash -> () -> L.ScriptContext -> Bool
mkPolicyFT tn pkh () ctx = traceIfFalse "unauthorized signature" validSignature &&
                           traceIfFalse "incorrect token name" validTokenName
  where
    info :: L.TxInfo
    info = L.scriptContextTxInfo ctx

    validSignature :: Bool
    validSignature = L.txSignedBy info $ L.unPaymentPubKeyHash pkh

    validTokenName :: Bool
    validTokenName = case flattenValue (L.txInfoMint info) of
      [(_, tn', _)] -> tn' == tn
      _             -> False

policyFT :: TokenName -> L.PaymentPubKeyHash -> Scripts.MintingPolicy
policyFT tn pkh = L.mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \tn' pkh' -> Scripts.mkUntypedMintingPolicy $ mkPolicyFT tn' pkh' ||])
  `PlutusTx.applyCode` PlutusTx.liftCode tn
  `PlutusTx.applyCode` PlutusTx.liftCode pkh

curSymbolFT :: TokenName -> L.PaymentPubKeyHash -> CurrencySymbol
curSymbolFT tn pkh = L.scriptCurrencySymbol $ policyFT tn pkh

-- NFTs

{-# INLINABLE mkPolicyNFT #-}
mkPolicyNFT :: Tx.TxOutRef -> TokenName -> () -> L.ScriptContext -> Bool
mkPolicyNFT oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount 
  where
    info :: L.TxInfo
    info = L.scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> L.txInInfoOutRef i == oref) $ L.txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (L.txInfoMint info) of
      [(_, tn', amt)] -> tn' == tn && amt == 1
      _               -> False

policyNFT :: Tx.TxOutRef -> TokenName -> Scripts.MintingPolicy
policyNFT oref tn = L.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.mkUntypedMintingPolicy $ mkPolicyNFT oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbolNFT :: Tx.TxOutRef -> TokenName -> CurrencySymbol
curSymbolNFT oref tn = L.scriptCurrencySymbol $ policyNFT oref tn

-- VAULTS (for custody of Ayllu tokens awaiting delivery)

-- Vault 1 (stores Ayllu tokens available to the teacher)

{-# INLINABLE mkVault1 #-}
mkVault1 :: L.PaymentPubKeyHash -> () -> () -> L.ScriptContext -> Bool
mkVault1 pkh () () ctx = traceIfFalse "unauthorized signature" validSignature
  where
    info :: L.TxInfo
    info = L.scriptContextTxInfo ctx

    validSignature :: Bool
    validSignature = L.txSignedBy info $ L.unPaymentPubKeyHash pkh

    -- TODO: remains to enforce that teacher returns of unused Ayllu tokens to Vault1    

data Typed1
instance Scripts.ValidatorTypes Typed1 where
  type instance DatumType Typed1    = ()
  type instance RedeemerType Typed1 = ()

typedVault1 ::  L.PaymentPubKeyHash -> Scripts.TypedValidator Typed1
typedVault1 pkh = Scripts.mkTypedValidator @Typed1 
  ($$(PlutusTx.compile [|| mkVault1 ||])
  `PlutusTx.applyCode` PlutusTx.liftCode pkh)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @() @()

vault1 :: L.PaymentPubKeyHash -> Scripts.Validator
vault1 = Scripts.validatorScript . typedVault1

vault1Hash :: L.PaymentPubKeyHash -> L.ValidatorHash
vault1Hash = L.validatorHash . vault1

vault1Address :: L.PaymentPubKeyHash -> L.Address
vault1Address = L.scriptHashAddress . vault1Hash

-- Vault 2 (stores Ayllu tokens awaiting to be retrieved by student)

{-# INLINABLE mkVault2 #-}
mkVault2 :: ValidationDatum -> () -> L.ScriptContext -> Bool
mkVault2 vd _ ctx = traceIfFalse "NFT validation failed" validateNFT
  where
    info :: L.TxInfo
    info = L.scriptContextTxInfo ctx

    nftCs :: CurrencySymbol
    nftCs = vdSymbol vd

    validateNFT :: Bool
    validateNFT = any isNftTk $ L.txInfoInputs info
      where
        isNftTk = any (== nftCs) . symbols . L.txOutValue . L.txInInfoResolved

data Typed2
instance Scripts.ValidatorTypes Typed2 where
  type instance DatumType Typed2    = ValidationDatum
  type instance RedeemerType Typed2 = ()

typedVault2 :: Scripts.TypedValidator Typed2
typedVault2 = Scripts.mkTypedValidator @Typed2
  $$(PlutusTx.compile [|| mkVault2 ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @ValidationDatum @()

vault2 :: Scripts.Validator
vault2 = Scripts.validatorScript typedVault2

vault2Hash :: L.ValidatorHash
vault2Hash = L.validatorHash vault2

vault2Address :: L.Address
vault2Address = L.scriptHashAddress vault2Hash
