{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Trace where

import Control.Monad          (void)
import Data.Text              (Text)
import Plutus.Contract        hiding (waitNSlots)
import Plutus.Trace.Emulator
import Wallet.Emulator.Wallet

import Setup
import Rewards
import Params


-- ENDPOINTS

endpoints :: Contract () RewardsSchema Text ()
endpoints = awaitPromise (mintAyllu' `select` mintTeacher' `select`
                          mintNFT' `select` adminTransfer' `select`
                          reward' `select` retrieve') >> endpoints
  where
    mintAyllu'     = endpoint @"mintAyllu" mintAyllu
    mintTeacher'   = endpoint @"mintTeacher" mintTeacher
    mintNFT'       = endpoint @"mintNFT" mintNFTUnsupervised
    adminTransfer' = endpoint @"adminTransfer" adminTransfer
    reward'        = endpoint @"reward" reward
    retrieve'      = endpoint @"retrieve" retrieve


-- Trace Emulator

test :: IO ()
test = runEmulatorTraceIO trace1

trace1 :: EmulatorTrace ()
trace1 = do
  let w1   = knownWallet 1 -- Administrator
      w2   = knownWallet 2 -- Teacher
      w3   = knownWallet 3 -- Student
      pkh1 = mockWalletPaymentPubKeyHash w1
      pkh2 = mockWalletPaymentPubKeyHash w2
      pkh3 = mockWalletPaymentPubKeyHash w3

  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  h3 <- activateContractWallet w3 endpoints

  callEndpoint @"mintAyllu" h1 $ MintParams
    { mpPKH    = pkh1
    , mpAmount = 1000
    }
  void $ waitNSlots 1

  callEndpoint @"mintTeacher" h2 $ MintParams
    { mpPKH    = pkh2
    , mpAmount = 300
    }
  void $ waitNSlots 1

  callEndpoint @"mintNFT" h3 $ NFTParams
    { npToken   = enrollmentTokenName
    , npAddress = mockWalletAddress w3
    }
  void $ waitNSlots 1

  callEndpoint @"adminTransfer" h1 $ AdminParams
    { apTeacherPKH  = pkh2
    , apAylluAmount = 50
    }
  void $ waitNSlots 1

  callEndpoint @"reward" h2 $ TeacherParams
    { tpStudentPKH  = pkh3
    , tpAdminPKH    = pkh1
    , tpAylluAmount = 10
    }
  void $ waitNSlots 2

  callEndpoint @"reward" h2 $ TeacherParams -- more rewards
    { tpStudentPKH  = pkh3
    , tpAdminPKH    = pkh1
    , tpAylluAmount = 25
    }
  void $ waitNSlots 2

  callEndpoint @"retrieve" h3 $ RetrieveParams
    { rpAdminPKH = pkh1 }
  void $ waitNSlots 1
