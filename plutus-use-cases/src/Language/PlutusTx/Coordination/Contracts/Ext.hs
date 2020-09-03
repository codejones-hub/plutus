{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
module Language.PlutusTx.Coordination.Contracts.Ext where

import Language.PlutusTx (CompiledCode, applyCode)
import qualified Language.PlutusTx                 as PlutusTx
import qualified Language.PlutusCore.Universe as PLC
import Ledger.Scripts (Script, fromCompiledCode, ScriptError, evaluateScript, Checking(Typecheck))
import           Language.PlutusTx.Prelude

data Arithmetic =
  Arithmetic
    { add :: Integer -> Integer -> Integer
    , sub :: Integer -> Integer -> Integer
    , mult :: Integer -> Integer -> Integer
    }

goodArithmetic :: CompiledCode PLC.DefaultUni Arithmetic
goodArithmetic = $$(PlutusTx.compile [|| Arithmetic{add = (+), sub = (-), mult = (*)} ||])

badArithmetic :: CompiledCode PLC.DefaultUni Arithmetic
badArithmetic = $$(PlutusTx.compile [||  Arithmetic{add = (*), sub = (*), mult = (+)} ||])

mkValidator :: CompiledCode PLC.DefaultUni (Arithmetic -> Integer)
mkValidator = $$(PlutusTx.compile [|| \Arithmetic{add, sub, mult} ->
  let r = ((add 12 15) `mult` 5) `sub` 10
  in if r == 125 then trace "correct" 125 else traceError "not correct"
  ||])

goodScript :: Script
goodScript = fromCompiledCode $ mkValidator `applyCode` goodArithmetic

badScript :: Script
badScript = fromCompiledCode $ mkValidator `applyCode` badArithmetic

{-
>>> scriptResult goodScript
>>> Right ["correct"]
>>> scriptResult badScript
>>> Left ["not correct"]
-}
scriptResult :: Script -> Either ScriptError [String]
scriptResult = evaluateScript Typecheck
