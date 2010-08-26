import Test.QuickCheck
import Sum

prop_bounded n = (n > 1) ==> sum35 n < sum [1 .. n - 1]
prop_compatible n = (n > 1) ==> sum35_elegant n == sum35 n

main = do
  quickCheck (prop_bounded :: Integer -> Property)
  quickCheck (prop_compatible :: Integer -> Property)
