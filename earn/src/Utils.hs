{-# LANGUAGE NoImplicitPrelude   #-}

module Utils where

import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger.Ada             (lovelaceValueOf)
import           Ledger.Value           as Value
import           Prelude                (Semigroup (..))


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

-- Does value contain specific token?
hasTokensOf :: CurrencySymbol -> TokenName -> Value -> Bool
hasTokensOf cs tn v = valueOf v cs tn > 0

-- Does value contain a token with given name?
hasTokenNamed :: TokenName -> Value -> Bool
hasTokenNamed tn v = foldr (||) False $ (\cs -> (valueOf v cs tn) > 0) <$> (symbols v)

-- Consolidate ([ScriptLookups a], TxConstraints i o)
consolidate :: (Semigroup a, Semigroup b, Monoid b) => (a, b) -> ([a], b) -> (a, b)
consolidate (a1, b1) (as, b2) = (foldl (<>) a1 as, b1 <> b2)


