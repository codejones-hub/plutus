There are 4 cases that break **plutus compilation** via the patched ghc:

## StrictData

When an ADT has at least one strict field.

## SameMod

Typeclasses that are defined and used *in the same module*.

Note: you cannot rely on plutus `compile` TemplateHaskell-helper, instead you have to use the more verbose `plc` marker,
because of TemplateHaskell's stage restriction.

## Orphan

This example is very similar to "SameMod" except the move of the
`class` definition outside in a separate file (named `Lib`) so to
as to change the original instance to an orphan. The Lib will compile, but at its
use site it breaks similarly to `SameMod`.

Note: you cannot rely on plutus `compile` TemplateHaskell-helper, instead you have to use the more verbose `plc` marker,
because of TemplateHaskell's stage restriction.


## OmitInterface

An example with separate modules Lib (for typeclass declaration and instances, no orphans) and Use (for plutus compilation/use site).

This breaks plutus compilation for some methods which belong to typeclasses of >=2 number of methods,
and the ghc compilation of the Lib is done with `-fomit-interface-pragmas`,
which happens to be the default flag for `ghc -O0` (not cabal).
