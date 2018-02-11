module Main where
import Test.QuickCheck
import ShareBill

import Data.List
import Data.Function

main :: IO()
main = quickCheck $
  property $ prop_SumOfAllSplitEqualToSumOfALLPositive


data Pair a b = Pair a b deriving (Eq, Show)
newtype SimpleTransactions = SimpleTransactions [(Name,Balance)] deriving (Eq,Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (a,b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

pairGenTransaction :: Gen (Name,Balance)
pairGenTransaction = pairGen

prop_SumOfAllSplitEqualToSumOfALLPositive :: SimpleTransactions -> Bool
prop_SumOfAllSplitEqualToSumOfALLPositive (SimpleTransactions l) = (totalTransfor - totalTransforDemand) < 100
  where
    totl = sum $ snd <$> l
    payByPersion = totl / (fromIntegral (length $ (groupBy ((==) `on` fst) l)))
    bal = (balanceSheet l payByPersion)
    totalTransforDemand =sum $ snd <$> filter (\x -> snd x > 0) bal
    totalTransfor = sum $ trd <$> (process bal)
    trd :: (a,a,b) -> b
    trd (_,_,x) = x

instance Arbitrary SimpleTransactions where
  arbitrary = sized $ \s -> do
                 n <- choose (0,s `min` 19)
                 xs <- vectorOf n arbitrary
                 return (SimpleTransactions xs)
  shrink (SimpleTransactions xs) = map SimpleTransactions (shrink xs)
