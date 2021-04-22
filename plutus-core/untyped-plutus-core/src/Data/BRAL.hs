{-# LANGUAGE LambdaCase #-}
module Data.BRAL ( BList -- abstract type for now
                 , nil
                 , cons
                 , index
                 , indexZero
                 , Data.BRAL.null
                 , Data.BRAL.head
                 , Data.BRAL.tail
                 ) where

import           Data.Bits (unsafeShiftR)


-- TODO: add doctests
-- TODO: quickcheck?

-- A complete binary tree.
-- Note: the size of the tree is not stored/cached,
-- unless it appears as a root tree in BList, which the size is stored inside the Cons.
data Tree a = Leaf a
            | Node a !(Tree a) !(Tree a)
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
    (Cons w1 t1 (Cons w2 t2 ts')) | w1 == w2 -> Cons (2*w1+1) (Node x t1 t2) ts'
    ts                                       -> Cons 1 (Leaf x) ts

-- O(1) worst-case
head :: BList a -> a
head (Cons 1 (Leaf x) _)     = x
head (Cons _ (Node x _ _) _) = x
head Nil                     = error "empty blist"
head _                       = error "invalid blist"

-- O(1) worst-case
tail :: BList a -> BList a
tail (Cons 1 (Leaf _) ts) = ts
tail (Cons treeSize (Node _ t1 t2) ts) =
    let halfSize = unsafeShiftR treeSize 1 -- probably faster than `div w 2`
    in Cons halfSize t1 $ Cons halfSize t2 ts -- split the node in two
tail Nil = error "empty blist"
tail _ = error "invalid blist"

-- 1-based
-- NOTE: no check if zero 0 index is passed, if 0 is passed it MAY overflow the index
-- TODO: use arch-independent Word32 or Word64
index :: BList a -> Word -> a
index Nil _ = error "out of bounds"
index (Cons w t ts) i =
    if i <= w
    then indexTree w i t
    else index ts (i-w)
  where
    indexTree :: Word -> Word -> Tree a -> a
    indexTree 1 1 (Leaf x) = x
    indexTree _ x (Leaf _) = error $ "out of bounds" ++ show x
    indexTree _ 1 (Node x _ _) = x
    indexTree treeSize offset (Node _ t1 t2 ) =
        let halfSize = unsafeShiftR treeSize 1 -- probably faster than `div w 2`
            offset' = offset - 1
        in if offset' <= halfSize
           then indexTree halfSize offset' t1
           else indexTree halfSize (offset' - halfSize) t2

-- 0-based
-- TODO: use arch-independent Word32 or Word64
indexZero :: BList a -> Word -> a
indexZero Nil _  = error "out of bounds"
indexZero (Cons w t ts) i  =
    if i < w
    then indexTree w i t
    else indexZero ts (i-w)
  where
    indexTree :: Word -> Word -> Tree a -> a
    indexTree 1 0 (Leaf x) = x
    indexTree _ _ (Leaf _) = error "out of bounds"
    indexTree _ 0 (Node x _ _) = x
    indexTree treeSize offset (Node _ t1 t2 ) =
        let halfSize = unsafeShiftR treeSize 1 -- probably faster than `div w 2`
        in if offset <= halfSize
           then indexTree halfSize (offset - 1) t1
           else indexTree halfSize (offset - 1 - halfSize) t2

-- TODO: safeIndex
