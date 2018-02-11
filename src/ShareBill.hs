{-# LANGUAGE TupleSections #-}
module ShareBill where

import Data.List
import Data.Function
import Data.Monoid
import Control.Arrow
import Control.Monad.Writer
import Numeric

type Name = String
type Balance = Float
type Ledger = (Name,Balance)
type PosLedger = (Name,Balance)
type NegLedger = (Name,Balance)
type Transfer = (Name,Name,Balance)

-- |The 'calculate' function is the main function to get transfer
calculate :: [String] -> [[String]] -> IO()
calculate namesRaw transactionsRaw= do
    let transactions = (\(x:_:y:_) -> (x, read (tail y)::Float)) <$> transactionsRaw
    let names = (,0) <$> namesRaw
    let total = sum $ snd <$> transactions
    let payByPersion = total / (fromIntegral (length names))
    mapM_ (putStrLn . display) (process $ balanceSheet (names <> transactions) payByPersion)
    where
      display (a,b,c) = a ++ " pays $" ++ formatCurrent c ++ " to " ++ b
      formatCurrent floatNum = showFFloat (Just 2) floatNum ""

-- |The 'balanceSheet' function is to get each person balance
balanceSheet :: [Ledger] -> Float -> [Ledger]
balanceSheet transactions  payByPersion = do
    let paymentCombination = fmap (\l -> (fst $ head l,sum $ snd <$> l)) (groupTransfers transactions)
    second (\x -> x - payByPersion) <$> paymentCombination
    where
      groupTransfers = groupBy ((==) `on` fst) . sort

-- |The 'process' function is to get each the transfers
process :: [Ledger] -> [Transfer]
process bal = (snd (divideAndConque $ sortBy (flip compare) (filterIgnored bal)))
  where
    filterIgnored = filter (not . ignored)
    ignored a
          | abs (snd a) <= 0.009 = True
          | otherwise = False

-- |The 'divideAndConque' function is to devide the Ledger by positive and negtive then compute
divideAndConque :: [Ledger] -> ((),[Transfer])
divideAndConque [] = ((),[])
divideAndConque l = let saperateList = partition (\x -> snd x<=0) l in
                   runWriter (checkAndCompute $ dynamicProceesing saperateList)

-- |The 'stepTransfer' function is for none optimazed sum zero group
stepTransfer :: ([NegLedger],[PosLedger]) -> Writer [Transfer] ()
stepTransfer ([],_) = return ()
stepTransfer (_,[]) = return ()
stepTransfer (x:xs,y:ys)
  | negate (snd x) < snd y = do
    tell [(fst x ,fst y, negate $ snd x)]
    stepTransfer (xs,insertIncreasing (second (+ (snd x)) y) ys)
  | negate (snd x) == snd y = do
    tell [(fst x,fst y, negate $ snd x)]
    stepTransfer (xs,ys)
  | otherwise = do
    tell [(fst x , fst y ,snd y)]
    stepTransfer (insertDecreasing (second (+ (snd y)) x) xs,ys)
     where
       insertIncreasing = insertBy (on compare snd)
       insertDecreasing = insertBy (flip compare `on` snd)

-- |The 'dynamicProceesing' function is to get zipper of all the sub rank N-pairs
dynamicProceesing :: ([NegLedger],[PosLedger]) -> [(([NegLedger],[NegLedger]),([PosLedger],[PosLedger]))]
dynamicProceesing (neg,pos) = sortByPair [(x,y)| x <- combine neg, y <- combine pos]
  where
    sublist = tail . subsequences
    combine x = zip (sublist x) (reverse $ sublist x)
    sortByPair = sortBy (on compare (\x -> (length $ fst x) + (length $ snd x)))

-- |The 'checkAndCompute' function is to check whether optiaml group exist and compute
checkAndCompute :: [(([NegLedger],[NegLedger]),([PosLedger],[PosLedger]))] -> Writer [Transfer] ()
checkAndCompute [] = return ()
checkAndCompute [(n,m)] = stepTransfer (fst n,fst m)
checkAndCompute (x:xs) = if checkZero pairs then
                           stepTransfer pairs >> (checkAndCompute $ dynamicProceesing remainder)
                         else
                           checkAndCompute xs
                         where
                           pairs = (fst $ fst x, snd $ fst x)
                           remainder = (fst $ snd x, snd $ snd x)

-- |The 'checkZero' function is to check the sub group total whether is 0
checkZero ::([NegLedger],[PosLedger]) -> Bool
checkZero (neg,pos) = if abs( sum (snd <$> neg) + sum (snd <$> pos)) <= 0.009 then
                        True
                      else
                        False
