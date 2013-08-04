module Primes
(
  primes
, primesUnder
) where

primes        :: (Integral a) => [a] -> [a]
primes []     = []
primes (x:xs) = x : primes (filter (\y -> y `mod` x /= 0) xs)

primesUnder   :: (Integral a) => a -> [a]
primesUnder n = 2 : (primes [3, 5 .. (n - 1)])