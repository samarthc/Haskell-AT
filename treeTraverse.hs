import Tree

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

searchZip :: Ord a => a -> Zipper a-> Maybe (Zipper a)
searchZip _ (EmptyTree, _) = Nothing
searchZip x arg@(Node val l r, crumbs)
    | x == val = Just arg
    | x < val = let newCrumb = LeftCrumb val r in searchZip x (l, newCrumb:crumbs)
    | x > val = let newCrumb = RightCrumb val l in searchZip x (r, newCrumb:crumbs)

search :: Ord a => a -> BSTree a -> Maybe (Zipper a)
search x tree = searchZip x (tree, [])