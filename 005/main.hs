-- 2520 is the smallest number that can be divided by each of the numbers
-- from 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of
-- the numbers from 1 to 20?

import Data.List

divisors :: [(Int, Int)]
divisors =  sort [ (2, 1)          -- 2
                 , (3, 1)          -- 3
                 , (2, 2)          -- 4
                 , (5, 1)          -- 5
                 , (2, 1), (3, 1)  -- 6
                 , (7, 1)          -- 7
                 , (2, 3)          -- 8
                 , (3, 2)          -- 9
                 , (2, 1), (5, 1)  -- 10
                 , (11, 1)         -- 11
                 , (2, 2), (3, 1)  -- 12
                 , (13, 1)         -- 13
                 , (2, 1), (7, 1)  -- 14
                 , (3, 1), (5, 1)  -- 15
                 , (2, 4)          -- 16
                 , (17, 1)         -- 17
                 , (2, 1), (3, 2)  -- 18
                 , (19, 1)         -- 19
                 , (2, 2), (5, 1)  -- 20
                 ]

impliedBy                      :: (Int, Int) -> [(Int, Int)] -> Bool
_ `impliedBy` []               =  False
(x, n) `impliedBy` ((y, m):zs) = (x == y && n <= m) || (x, n) `impliedBy` zs

needed                 :: [(Int, Int)] -> [(Int, Int)]
needed []              = []
needed (x:xs)
    | x `impliedBy` xs = needed xs
    | otherwise        = x : needed xs

result = foldr1 (*) $ map (\(m, n) -> m ^ n) $ needed divisors

main = do
  putStrLn $ show result