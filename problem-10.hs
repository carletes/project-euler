-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.

import Primes

result = sum $ primesUnder 100000

main = do
  putStrLn $ show result