-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
--
--   a^2 + b^2 = c^2
--
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- Given the constraints:
--
--     a^2 + b^2 = c^2
--     a + b + c = 1000
--
-- it follows that
--
--     a /= b (because they have to be natural numbers)
--
-- and
--
--     a < 1000
--     b < 1000
--
-- and, by substituting [c = 1000 - (a + b)] in [a^2 + b^2 = c^2],
--
--     1000(a + b) = 500000 + ab
 
pairs         :: [Int] -> [(Int, Int)]
pairs (x:[y]) = [(x, x), (x, y), (y, y)]
pairs (x:xs)  = (x, x) : (zip [x, x ..] xs) ++ pairs xs
pairs _       = error "pairs: Invalid input"

validPair        :: (Int, Int) -> Bool
validPair (a, b) = 1000 * (a + b) == 500000 + a * b

result = (a, b, c, abc) where
    (a, b) = head $ dropWhile (not . validPair) (pairs [1 .. 1000])
    c = truncate $ sqrt (a' ** 2 + b' ** 2)
    abc = a * b * c
    a' = fromIntegral a::Double
    b' = fromIntegral b::Double

main = do
  putStrLn (show result)
  