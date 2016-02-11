import Data.List (groupBy)
import Data.Function (on)
import Data.Char (isSpace)

--words1 first groups non-space characters together, producing a list of words interspersed with spaces. It then filtes the list to weed out the spaces.
words1 :: String -> [String]
words1 line = filter (not . (any isSpace)) $ groupBy ( (&&) `on` not . isSpace) line

--split takes a predicate and a list. The list is split at the elements satisfying the predicate to produce a list of lists.
--Example: split (=='-') "This-could-be-a-valid-file-name" would produce ["This", "could", "be", "a", "valid", "file", "name"]
--Note: splitFirst p ls would contain null lists where multiple consecutive elements satisying p occurred.
splitFirst :: (a -> Bool) -> [a] -> [[a]]
splitFirst _ [] = []
splitFirst p x = case break p x of
                (f, n:next) -> f : (split p next)
                (f, []) -> [f]

split p ls = filter (not.null) $ splitFirst p ls

words2 :: String -> [String]
words2 =  split isSpace
