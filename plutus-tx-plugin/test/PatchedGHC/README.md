# Issues with the Patched GHC

There are 3 cases that break **plutus compilation** via the patched ghc:

## StrictData

When an ADT has at least one strict field.

## InstanceSameMod

Instances (no matter the number of methods) which are defined *and* compiled by plutus *inside the same module*.

Note: you cannot rely on plutus `compile` TemplateHaskell-helper, instead you have to use the more verbose `plc` marker,
because of TemplateHaskell's stage restriction.

## OmitInterface

An example with separate modules Lib (for typeclass declaration and instances, no orphans) and Use (for plutus compilation/use site).

This breaks plutus compilation for some methods which belong to typeclasses of >=2 number of methods,
and the ghc compilation of the Lib is done with `-fomit-interface-pragmas`,
which happens to be the default flag for `ghc -O0` (not cabal).
