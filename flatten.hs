module Flatten
(flatten)
where

flatten :: [[a]] -> [a]

flatten [] = []
flatten (xs:xxs) = xs ++ flatten xxs