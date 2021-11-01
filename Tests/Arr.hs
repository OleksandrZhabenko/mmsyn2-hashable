module Main where

import CaseBi.Arr
import Control.DeepSeq (force)

main = do
  mapM_ print . map (getBFstLSorted' 0 . zip (force [-1001..2000]::[Int]) $ [5000..]) $ concat (replicate 10 [-10000..10000])
