{-# LANGUAGE NoImplicitPrelude     #-}

module Params where

import Ledger                  as L
import Ledger.Value
import Ledger.Ada
import PlutusTx.Prelude
import PlutusTx.Builtins.Class

-- Token Names

teacherTokenName :: TokenName
teacherTokenName = TokenName $ stringToBuiltinByteString "TEACHER"

aylluTokenName :: TokenName
aylluTokenName = TokenName $ stringToBuiltinByteString "AYLLU"

enrollmentTokenName :: TokenName
enrollmentTokenName = TokenName $ stringToBuiltinByteString "ENROLLED"

-- Global parameters

valMinAda :: Value
valMinAda = toValue L.minAdaTxOut


