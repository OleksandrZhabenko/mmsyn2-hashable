{-# LANGUAGE BangPatterns #-}

module Main where


import Case.Hashable.Cuckoo
import Control.DeepSeq (force)

main = do
  mapM_ print . getBFstLLSized' 3002 (0::Int) (zip lss [5000..]) $ concat (replicate 10 [-10000..10000])
    where !lss = force ([-1001..2000]::[Int])
