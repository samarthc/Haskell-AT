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