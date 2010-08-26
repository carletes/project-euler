module Sum where

-- Brute force approach
sum35 :: (Integral a) => a -> a
sum35 n = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1 .. n - 1]

-- (3, 5) = 1, plus some Haskell idioms
sum35_elegant :: (Integral a) => a -> a
sum35_elegant n = sum [3, 6 .. n - 1] + 
                  sum [5, 10 .. n - 1] - 
                  sum [15, 30 .. n - 1]
