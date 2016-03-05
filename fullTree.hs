data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

instance Functor Tree where
fmap func EmptyTree = EmptyTree
fmap func (Node x left right) = Node (func x) (fmap func left) (fmap func right)

{-instance Applicative Tree where
pure a = Node a EmptyTree EmptyTree
EmptyTree <*> _ = EmptyTree
_ <*> EmptyTree = EmptyTree
(Node f l1 r1) <*> (Node x l2 r2) = Node (f x) (l1 <*> l2) (r1 <*> r2)
-}
