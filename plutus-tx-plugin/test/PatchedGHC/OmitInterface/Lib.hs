-- The following flag (default  with -O0) breaks some methods of N-classes (non-singleton-classes)
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module PatchedGHC.OmitInterface.Lib where

-- Singleton class works fine
class C a where
    m :: a -> ()
instance C Integer where
    m _ = ()
instance C Bool where
    m _ = ()

-- BROKEN for N-classes (>=2 number of methods)
class TwoMethodClass a where
    methodOne :: a -> Integer
    methodTwo :: a -> Bool
instance TwoMethodClass Integer where
    methodOne x = 0
    methodTwo _ = False
instance TwoMethodClass Bool where
    methodOne _ = 1
    methodTwo x = True

