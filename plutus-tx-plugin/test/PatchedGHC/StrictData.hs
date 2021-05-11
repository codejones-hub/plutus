{-# LANGUAGE BangPatterns #-}
module PatchedGHC.StrictData where

import           PlutusTx

---- WORKING EXAMPLE
--------------------
data T1 a =
    -- data-constructor with all lazy fields is ok
    T1D a a

ok1 :: CompiledCode (T1 Integer)
ok1 = $$(compile [|| T1D 1 2 ||])

-- BangPatterns is fine
f :: T1 a -> a
f (T1D !x y) = x

ok2 = $$(compile [|| f :: T1 Integer -> Integer ||])


--- BROKEN EXAMPLE
------------------

data T2 a =
    -- data-constructor with at least one STRICT field breaks
    T2D !a a

-- GHC panic: interface file is missing entry for name:$WT2D
-- These options do not help. {-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas #-}
er1 = $$(compile [|| T2D 1 2 :: T2 Integer ||])
