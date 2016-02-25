import Data.Char
main = interact respondPalindrome

respondPalindrome :: String -> String
respondPalindrome = unlines . map response . lines
    where response s = if (isPalindrome s) then "Palindrome" else "Not a palindrome. Idiot."

isPalindrome :: String -> Bool
isPalindrome x = let s = map toLower x in s == reverse s
