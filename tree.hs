infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ a = a
a .++ Empty = a
(x :-: xs) .++ ys = x :-: (xs .++ ys)

--instance (Show a) => List a where


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

treeDepth :: Tree a -> Integer
treeDepth EmptyTree = 0
treeDepth (Node a left right) = 1 + max (treeDepth left) (treeDepth right)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)