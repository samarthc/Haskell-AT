module Tree where

import qualified Data.Foldable as F

data BSTree a = EmptyTree | Node a (BSTree a) (BSTree a) deriving (Eq, Show, Read) --Binary Search Tree

instance Functor BSTree where
    fmap func EmptyTree = EmptyTree
    fmap func (Node x left right) = Node (func x) (fmap func left) (fmap func right)

{-instance Applicative BSTree where
    pure a = Node a EmptyTree EmptyTree
    EmptyTree <*> _ = EmptyTree
    _ <*> EmptyTree = EmptyTree
    (Node f l1 r1) <*> (Node x l2 r2) = Node (f x) (l1 <*> l2) (r1 <*> r2)
-}

instance F.Foldable BSTree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x left right) = foldMap f left `mappend` f x `mappend` foldMap f right

singleton :: a -> BSTree a
singleton x = Node x EmptyTree EmptyTree

insert :: (Ord a) => a -> BSTree a -> BSTree a
insert x EmptyTree = singleton x
insert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (insert x left) right
    | x > a = Node a left (insert x right)

{-elem :: (Ord a) => a -> BSTree a -> Bool
elem _ EmptyTree = False
elem x (Node a left right)
    | x == a = True
    | x < a = elem x left
    | x > a = elem x right
-}
--elem is part of Foldable

depth :: BSTree a -> Integer
depth EmptyTree = 0
depth (Node a left right) = 1 + max (depth left) (depth right)

fromList :: (Ord a) => [a] -> BSTree a
fromList = foldr insert EmptyTree