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

module Utils where

import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger.Ada             as Ada
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String, last, undefined)


-- Remove all Ada from value
removeAda :: Value -> Value
removeAda v = let
    adaValue = valueOf v adaSymbol adaToken
    negateAdaValue = lovelaceValueOf . negate $ adaValue
  in
    v <> negateAdaValue

-- Does value contain non-Ada tokens?
hasTokens :: Value -> Bool
hasTokens = not . Value.isZero . removeAda

