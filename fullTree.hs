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

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = singleton x
insert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

elem :: (Ord a) => a -> Tree a -> Bool
elem _ EmptyTree = False
elem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

depth :: Tree a -> Integer
depth EmptyTree = 0
depth (Node a left right) = 1 + max (treeDepth left) (treeDepth right)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert EmptyTree