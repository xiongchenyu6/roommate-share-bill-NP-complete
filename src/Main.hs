module Main where

import ShareBill (calculate)

main :: IO ()
main = do
    namesRaw <- lines <$> readFile "./data/name.txt"
    transactionsRaw <-(fmap.fmap) words lines <$> readFile "./data/transactions.txt"
    calculate namesRaw transactionsRaw
