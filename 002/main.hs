-- Each new term in the Fibonacci sequence is generated by adding the
-- previous two terms. By starting with 1 and 2, the first 10 terms
-- will be:
--
--     1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
--
-- By considering the terms in the Fibonacci sequence whose values do 
-- not exceed four million, find the sum of the even-valued terms.

-- ``Int`` is much faster that ``Integer``, and, in our case (n=4,000,000)
-- big enough to fit the result without overflow

module Main (main) where

import System.Environment
import System.IO

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 2
fib n = fib (n - 1) + fib (n - 2)

fibsUnder :: Integer -> [Integer]
fibsUnder n = takeWhile (\i -> i <= n) (map fib [1 ..])

result n = sum $ filter even $ fibsUnder n

main = do
  args <- getArgs
  prog <- getProgName
  if not (null args)
     then let n = read $ head args
              r = result n
          in (putStr $ (show r) ++ "\n")
    else
    hPutStr stderr $ "Usage: " ++ prog ++ " <n>\n"
    