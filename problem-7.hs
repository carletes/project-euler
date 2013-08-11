-- By listing the first six prime numbers:
--
--     2, 3, 5, 7, 11, 13
--
-- we can see that the 6th prime is 13.
--
-- What is the 10 001st prime number?

import Numbers

result = head $ drop 9999 $ primes [3, 5 ..]

main = do
  putStrLn $ show result