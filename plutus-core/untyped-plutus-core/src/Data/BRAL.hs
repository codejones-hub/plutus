{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.BRAL ( BList -- abstract type for now
                 , nil
                 , cons
                 , index
                 , Data.BRAL.null
                 , Data.BRAL.uncons
                 ) where

import           Data.Bits (unsafeShiftR)

import qualified Data.List as List
import GHC.Exts as Exts

instance Exts.IsList (BList a) where
  type Item (BList a) = a
  fromList = foldr cons Nil
  toList = List.unfoldr uncons

-- TODO: add doctests
-- TODO: quickcheck?

-- A complete binary tree.
-- Note: the size of the tree is not stored/cached,
-- unless it appears as a root tree in BList, which the size is stored inside the Cons.
data Tree a = Leaf a
            | Node !Word a !(Tree a) !(Tree a)
            deriving Show

-- a strict list of complete binary trees accompanied by their size.
-- The trees appear in >=-size order.
-- Note: this blist is strict on its spine, unlike the haskell's stdlib list
data BList a = Cons
               -- TODO: use arch-independent Word32 or Word64
               !Word -- ^ the size of the head tree
               !(Tree a) -- ^ the head tree
               !(BList a) -- ^ the tail trees
             | Nil
             deriving Show

{-# INLINABLE nil #-}
nil :: BList a
nil = Nil

{-# INLINABLE null #-}
null :: BList a -> Bool
null Nil = True
null _   = False

-- O(1) worst-case
cons :: a -> BList a -> BList a
cons x = \case
    (Cons w1 t1 (Cons w2 t2 ts')) | w1 == w2 -> Cons (2*w1+1) (Node w1 x t1 t2) ts'
    ts                                       -> Cons 1 (Leaf x) ts

uncons :: BList a -> Maybe (a, BList a)
uncons (Cons _ (Node s a l r) ys) = Just (a, Cons s l (Cons s r ys))
uncons (Cons _ (Leaf a) ys)       = Just (a, ys)
uncons Nil                        = Nothing

-- TODO: use arch-independent Word32 or Word64
index :: BList a -> Word -> a
index Nil _ = error "out of bounds"
index (Cons w t ts) !i0 =
    if i0 >= w
    then index ts (i0-w)
    else indexTree i0 t
  where
    indexTree :: Word -> Tree a -> a
    indexTree 0 (Leaf x) = x
    indexTree x (Leaf _) = error $ "out of bounds" ++ show x
    indexTree i (Node s x l r)
        | i == 0 = x
        | i <= s = indexTree (i-1) l
        | otherwise = indexTree (i-1-s) r

-- TODO: safeIndex
-- TODO: add monoid instance?
