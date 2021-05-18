module PatchedGHC.InstanceContext.Lib where

import qualified PlutusTx.Builtins      as Builtins

class Sized a where
    size :: a -> Integer

instance Sized Integer where
    size x = x

-- broken because of context
-- the size  and the contents of the context do not seem to matter in this breakage.
instance (Sized a, Sized b) => Sized (a, b) where
    {-# NOINLINE size #-}
    size (a, b) = size a `Builtins.addInteger` size b

-- this instead works because of NO CONTEXT
-- instance Sized (a, b) where
--     {-# NOINLINE size #-}
--     size (a, b) = 3

-- FlexibleInstance with NO CONTEXT also works
-- instance Sized (Integer, Integer) where
--     {-# NOINLINE size #-}
--     size (a, b) = size a `Builtins.addInteger` size b

