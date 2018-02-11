{-# LANGUAGE TupleSections #-}
module Main where
import Data.List
import Data.Function
import Data.Monoid
import Control.Arrow
import Data.Ord
import Control.Monad.Writer
import Numeric

type Name = String
type Balance = Float
type Transcation = (Name,Name,Balance)

formatCurrent floatNum = showFFloat (Just 2) floatNum ""

display (a,b,c) = a ++ " pays $" ++ formatCurrent c ++ " to " ++ b
main :: IO ()
main = do
    namesRow <- lines <$> readFile "./data/name.txt"
    transactionsRaw <-(fmap.fmap) words lines <$> readFile "./data/transactions.txt"
    let transactions = (\(x:y:z:xs) -> (x, read (tail z)::Float)) <$> transactionsRaw
    let names = (,0) <$> namesRow
    let total = sum $ snd <$> transactions
    let payByPersion = total / (fromIntegral (length names))
    let paymentCombination = fmap (\l -> (fst $ head l,sum $ snd <$> l)) (groupTranscations $ names <> transactions)
    let summary = second (\x -> x - payByPersion) <$> paymentCombination
    mapM_ (putStrLn . display) (snd (basicProcess $ sortBy (flip compare) (filterIgnored summary)))
    where
      groupTranscations = groupBy ((==) `on` fst) . sort

filterIgnored = filter (not . ignored)

sortByBalance :: [(Name,Balance)] -> [(Name,Balance)]
sortByBalance = sortBy (compare `on` snd)

ignored :: (Name,Balance) -> Bool
ignored a
   | abs (snd a) <= 0.009 = True
   | otherwise = False

basicProcess :: [(Name,Balance)] -> ((),[Transcation])
basicProcess [] = ((),[])
basicProcess l = let saperateList = partition (\x -> snd x<=0) l in
                   runWriter (checkAndCompute $ dynamicProceesing saperateList)

mapTuple f = f *** f

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

stepTransfor' :: ([(Name,Balance)],[(Name,Balance)]) -> Writer [Transcation] ()
stepTransfor' ([],_) = return ()
stepTransfor' (_,[]) = return ()
stepTransfor' (x:xs,y:ys)
  | negate (snd x) < snd y = do
    tell [(fst x ,fst y, negate $ snd x)]
    stepTransfor' (xs,insertIncreasing (second (+ (snd x)) y) ys)
  | negate (snd x) == snd y = do
    tell [(fst x,fst y, negate $ snd x)]
    stepTransfor' (xs,ys)
  | otherwise = do
    tell [(fst x , fst y ,snd y)]
    stepTransfor' (insertDecreasing (second (+ (snd y)) x) xs,ys)
     where
       insertIncreasing = insertBy (on compare snd)
       insertDecreasing = insertBy (flip compare `on` snd)

dynamicProceesing (neg,pos) = sortByPair [(x,y)| x <- combine neg, y <- combine pos]
  where
    sublist = tail . subsequences
    combine x = zip (sublist x) (reverse $ sublist x)
    sortByPair = sortBy (on compare (\x -> (length $ fst x) + (length $ snd x)))

checkAndCompute :: [(([(Name,Balance)],[(Name,Balance)]),([(Name,Balance)],[(Name,Balance)]))] -> Writer [Transcation] ()
checkAndCompute [(n,m)] = stepTransfor' (fst n,fst m)
checkAndCompute (x:xs) = if checkZero pairs then
                           stepTransfor' pairs >> (checkAndCompute $ dynamicProceesing remainder)
                         else
                           checkAndCompute xs
                         where
                           pairs = (fst $ fst x, snd $ fst x)
                           remainder = (fst $ snd x, snd $ snd x)


checkZero ::([(Name,Balance)],[(Name,Balance)]) -> Bool
checkZero (neg,pos) = if abs( sum (snd <$> neg) + sum (snd <$> pos)) <= 0.009 then
                        True
                      else
                        False