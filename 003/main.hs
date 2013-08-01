-- The prime factors of 13195 are 5, 7, 13 and 29.
-- 
-- What is the largest prime factor of the number 600851475143?
--
-- Explore! http://www.haskell.org/haskellwiki/Prime_numbers

factorBound :: Integer -> Integer
factorBound = floor . sqrt . fromIntegral

sieveFactors :: Integer -> [Integer]
sieveFactors n = sieve0 [2 .. b] where
  b = factorBound n
  -- ``sieve0``: All divisors of ``n`` in the Eratosthenes sieve, 
  -- up to ``floor(sqrt(n))``
  sieve0 [] = []
  sieve0 (x:xs) = x : sieve0 (filter (\i -> i `mod` x /= 0 && n `mod` i == 0 ) xs)
  
main = do
  print $ sieveFactors 600851475143