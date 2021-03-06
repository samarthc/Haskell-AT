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

toList :: BSTree a -> [a]
toList = foldMap (\x -> [x])

data Crumb a = LeftCrumb a (BSTree a) | RightCrumb a (BSTree a) deriving (Show)
type Breadcrumbs a = [Crumb a]
type Zipper a = (BSTree a, Breadcrumbs a)

goRight :: Zipper a -> Zipper a
goRight arg@(EmptyTree, _) = arg
goRight (Node x l r, trail) = (r, newCrumb:trail)
    where newCrumb = RightCrumb x l

goLeft :: Zipper a -> Zipper a
goLeft arg@(EmptyTree, _) = arg
goLeft (Node x l r, trail) = (l, newCrumb:trail)
    where newCrumb = LeftCrumb x r

goUp :: Zipper a -> Zipper a
goUp arg@(EmptyTree, _) = arg
goUp arg@(_, []) = arg
goUp (cur, RightCrumb p l:cs) = (Node p l cur, cs)
goUp (cur, LeftCrumb p r:cs) = (Node p cur r, cs)

sibling :: Zipper a -> Zipper a
sibling arg@(EmptyTree, _) = arg
sibling arg@(_, RightCrumb _ _:_) = goLeft . goUp $ arg
sibling arg@(_, LeftCrumb _ _:_) = goRight . goUp $ arg

root :: Zipper a -> BSTree a
root (tree, []) = tree
root pair = root . goUp $ pair

modify :: (a -> a) -> Zipper a -> Zipper a
modify _ arg@(EmptyTree, _) = arg
modify f (Node x l r, crumbs) = (Node (f x) l r, crumbs)

search :: Ord a => a -> BSTree a -> Maybe (Zipper a)
search x tree = searchZip x (tree, [])
    where
    searchZip :: Ord a => a -> Zipper a -> Maybe (Zipper a)
    searchZip _ (EmptyTree, _) = Nothing
    searchZip x arg@(Node val l r, crumbs)
        | x == val = Just arg
        | x < val = let newCrumb = LeftCrumb val r in searchZip x (l, newCrumb:crumbs)
        | x > val = let newCrumb = RightCrumb val l in searchZip x (r, newCrumb:crumbs)
