module PatchedGHC.InstanceContext.Use where

import Data.Proxy
import PlutusTx
import PlutusTx.Plugin.Utils (plc)

import PatchedGHC.InstanceContext.Lib

-- GHC panic: interface file is missing entry for name:$dSized_a4bP
er1 :: CompiledCode (Integer -> Integer -> Integer)
er1 = plc (Proxy @"er1") (\a b -> size (a, b))


