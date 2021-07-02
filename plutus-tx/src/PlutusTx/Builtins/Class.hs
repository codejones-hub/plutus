{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module PlutusTx.Builtins.Class where

import           Data.Kind

import           Data.ByteString            as BS
import           PlutusTx.Builtins.Internal

import           Data.String                (IsString (..))

import qualified GHC.Magic                  as Magic

{-|
A class indicating that the given type is representable as a Plutus Core builtin type.

For example, Plutus Core has builtin booleans, but the Haskell 'Bool' type can also be
compiled into Plutus Core as a datatype. The 'IsBuiltin Bool' instance allows us to
convert between those in on-chain code.
-}
class IsBuiltin a where
    type BuiltinRep a :: Type

    fromBuiltin :: BuiltinRep a -> a
    toBuiltin :: a -> BuiltinRep a

instance IsBuiltin Integer where
    type instance BuiltinRep Integer = BuiltinInteger
    {-# INLINABLE fromBuiltin #-}
    fromBuiltin = id
    {-# INLINABLE toBuiltin #-}
    toBuiltin = id

instance IsBuiltin Bool where
    type instance BuiltinRep Bool = BuiltinBool
    {-# INLINABLE fromBuiltin #-}
    fromBuiltin b = ifThenElse b True False
    {-# INLINABLE toBuiltin #-}
    toBuiltin b = if b then true else false

instance IsBuiltin () where
    type instance BuiltinRep () = BuiltinUnit
    {-# INLINABLE fromBuiltin #-}
    fromBuiltin _ = ()
    {-# INLINABLE toBuiltin #-}
    toBuiltin _ = unitval

instance IsBuiltin ByteString where
    type instance BuiltinRep ByteString = BuiltinByteString
    {-# INLINABLE fromBuiltin #-}
    fromBuiltin = id
    {-# INLINABLE toBuiltin #-}
    toBuiltin = id

instance IsBuiltin Char where
    type instance BuiltinRep Char = BuiltinChar
    {-# INLINABLE fromBuiltin #-}
    fromBuiltin = id
    {-# INLINABLE toBuiltin #-}
    toBuiltin = id

{- Note [noinline hack]
For some functions we have two conflicting desires:
- We want to have the unfolding available for the plugin.
- We don't want the function to *actually* get inlined before the plugin runs, since we rely
on being able to see the original function for some reason.

'INLINABLE' achieves the first, but may cause the function to be inlined too soon.

We can solve this at specific call sites by using the 'noinline' magic function from
GHC. This stops GHC from inlining it. As a bonus, it also won't be inlined if
that function is compiled later into the body of another function.

We do therefore need to handle 'noinline' in the plugin, as it itself does not have
an unfolding.
-}

-- We can't put this in `Builtins.hs`, since that force `O0` deliberately, which prevents
-- the unfoldings from going in. So we just stick it here. Fiddly.
instance IsString BuiltinString where
    -- Try and make sure the dictionary selector goes away, it's simpler to match on
    -- the application of 'stringToBuiltinString'
    {-# INLINE fromString #-}
    -- See Note [noinline hack]
    fromString = Magic.noinline stringToBuiltinString

{-# INLINABLE stringToBuiltinString #-}
stringToBuiltinString :: String -> BuiltinString
stringToBuiltinString = go
    where
        go []     = emptyString
        go (x:xs) = charToString x `appendString` go xs

instance IsBuiltin BuiltinString where
    type instance BuiltinRep BuiltinString = BuiltinString
    {-# INLINABLE fromBuiltin #-}
    fromBuiltin = id
    {-# INLINABLE toBuiltin #-}
    toBuiltin = id
