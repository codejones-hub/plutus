module PatchedGHC.OmitInterface.Use where

import           PlutusTx

import           PatchedGHC.OmitInterface.Lib

-- Works fine for singleton classes
ok1 = $$(compile [|| m (1 :: Integer) ||])

-- Surprisingly this works fine.
ok2 = $$(compile [|| methodTwo True ||])

-- The following breaks if the **Lib** uses -fomit-interface-pragmas, default on -O0
--
-- What happens is that we query for the unfolding of the methodOne method-wrapper,
-- and get back some ghc core of `methodOne :: t = rhs`.
-- The type t is correct but the rhs has wrong implementation inside.
-- The implementation actually is the same as of `methodTwo` method-wrapper,
-- which in the best case will yield a type error when we try to typecheck using our plugin,
-- or in worst case will dispatch the methodTwo instead of the methodOne silently.
-- The worst case happens if the involved methods have the same type signature.
er1 = $$(compile [|| methodOne (3 :: Integer) ||])
