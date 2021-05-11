module PatchedGHC.SameMod where

import           PlutusTx
import PlutusTx.Plugin.Utils (plc)
import Data.Proxy

-- BROKEN for both 1-method-classes and N-method-classes
class C a where
    m :: a -> ()

instance C Integer where
    m _ = ()

instance C Bool where
    m _ = ()

-- GHC panic:         interface file is missing entry for name:m
er1 = plc (Proxy @"er1") (m (1 :: Integer))

-- GHC panic:         interface file is missing entry for name:m
er2 = plc (Proxy @"er2") (m True)
