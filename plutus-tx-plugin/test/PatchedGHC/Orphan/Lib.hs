-- The following flag (default  with -O0) breaks N-class (non-singleton-class) methods
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module PatchedGHC.Orphan.Lib where

-- BROKEN for both 1-method-classes and N-method-classes
class C a where
    m :: a -> ()
