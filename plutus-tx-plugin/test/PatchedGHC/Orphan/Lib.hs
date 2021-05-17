module PatchedGHC.Orphan.Lib where

-- BROKEN for both 1-method-classes and N-method-classes
class C a where
    m :: a -> ()
