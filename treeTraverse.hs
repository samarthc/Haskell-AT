import Tree

data Crumb a = Left a (BSTree a) | Right a (BSTree a)
type Breadcrumbs a = [Crumb a]

goRight :: (BSTree a, Breadcrumbs a) -> (BSTree a, Breadcrumbs a)
goRight arg@(EmptyTree, _) = arg
goRight (Node x l r, trail) = (r, newCrumb:trail)
    where newCrumb = Right x l

goLeft :: (BSTree a, Breadcrumbs a) -> (BSTree a, Breadcrumbs a)
goLeft arg@(EmptyTree, _) = arg
goLeft (Node x l r, trail) = (l, newCrumb:trail)
    where newCrumb = Left x r

goUp :: (BSTree a, Breadcrumbs a) -> (BSTree a, Breadcrumbs a)
goUp arg@(EmptyTree, _) = arg
goUp arg@(_, []) = arg
goUp (cur, Right p l:cs) = (Node p l cur, cs)
goUp (cur, Left p r:cs) = (Node p cur r, cs)

sibling :: (BSTree a, Breadcrumbs a) -> (BSTree a, Breadcrumbs a)
sibling arg@(EmptyTree, _) = arg
sibling arg@(_, Right _ _:_) = goLeft . goUp $ arg
sibling arg@(_, Left _ _:_) = goRight . goUp $ arg

root :: (BSTree a, Breadcrumbs a) -> BSTree a
root (tree, []) = tree
root pair = root . goUp $ pair