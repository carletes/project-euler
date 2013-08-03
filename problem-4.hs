-- A palindromic number reads the same both ways. The largest palindrome
-- made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Data.List

isPalindrome        :: String -> Bool
isPalindrome []     =  True
isPalindrome [x]    =  True
isPalindrome (x:xs) =   (x == last xs) && (isPalindrome $ init xs)

pairs            :: Int -> Int -> [(Int, Int)]
pairs top bottom = zip [top, top ..] [top, top - 1 .. bottom]

allPairs                            :: Int -> Int -> [(Int, Int)]
allPairs top bottom | top == bottom = pairs top bottom
allPairs top bottom                 = pairs top bottom ++ allPairs (top - 1) bottom

result            :: Int -> Int -> String
result top bottom = head $ dropWhile (not . isPalindrome) (map show products) where
    products = sortBy (\a b -> compare b a) (map (\(x, y) -> x * y) $ allPairs top bottom)

main = do
  putStrLn $ show $ result 999 100
