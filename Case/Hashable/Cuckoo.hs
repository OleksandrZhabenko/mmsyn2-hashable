-- |
-- Module      :  Case.Hashable.Cuckoo
-- Copyright   :  (c) OleksandrZhabenko 2021
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- A library that can be used as a @case ... of@ constuction analogue for the Hashable keys.
-- For the large lists can be more time efficient than CaseBi.Arr.getBFst' analogue. For the lists of 'Int's
-- the benchmarks does not show significant improvements, but it definitely uses more memory if linked statically.
-- If you plan to use it together with the former one, please, use qualified import to avoid names ambiguity.


{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Case.Hashable.Cuckoo where

import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H (fromList, fromListWithSizeHint)
import GHC.ST
import GHC.Magic (runRW# )
import Data.Maybe (fromMaybe)
import Data.Hashable (Hashable(..))
import GHC.Arr

getBFstL' :: (Eq k, Hashable k) => v -> [(k, v)] -> k -> v
getBFstL' def pairs key = fromMaybe def .
  (\(ST st_rep) -> case runRW# st_rep of (# _, a #) -> a) . -- Actually is rewritten from the GHC.ST.runST to remove the forall constraint
    lookup2 pairs $ key
{-# INLINE getBFstL' #-}

lookup2 pairs key = H.fromList pairs >>= \ht -> C.lookup ht key
{-# INLINE lookup2 #-}

lookup2Sized n pairs key = H.fromListWithSizeHint n pairs >>= \ht -> C.lookup ht key
{-# INLINE lookup2Sized #-}

lookupL pairs keys = H.fromList pairs >>= \ht -> mapM (C.lookup ht) keys
{-# INLINE lookupL #-}

lookupLSized n pairs keys = H.fromListWithSizeHint n pairs >>= \ht -> mapM (C.lookup ht) keys
{-# INLINE lookupLSized #-}

getBFstLL' :: (Eq k, Hashable k) => b -> [(k, b)] -> [k] -> [b]
getBFstLL' def pairs keys =
 (\(ST st_rep) -> case runRW# st_rep of (# _, a #) -> map (\x -> case x of { Just y -> y; ~rr -> def }) a)
   (lookupL pairs keys)
{-# INLINE getBFstLL' #-}

getBFstLArr' :: (Hashable k, Ix i, Eq k) => b -> [(k, b)] -> Array i k -> Array i b
getBFstLArr' def pairs keysArr =
 (\(ST st_rep) -> case runRW# st_rep of (# _, a #) -> amap (\x -> case x of { Just y -> y; ~rr -> def }) a)
   (lookupL pairs keysArr)
{-# INLINE getBFstLArr' #-}

getBFstLSized' :: (Eq k, Hashable k) => Int -> v -> [(k, v)] -> k -> v
getBFstLSized' n def pairs key = fromMaybe def .
  (\(ST st_rep) -> case runRW# st_rep of (# _, a #) -> a) . -- Actually is rewritten from the GHC.ST.runST to remove the forall constraint
    lookup2Sized n pairs $ key
{-# INLINE getBFstLSized' #-}

getBFstLLSized' :: (Eq k, Hashable k) => Int -> b -> [(k, b)] -> [k] -> [b]
getBFstLLSized' n def pairs keys =
 (\(ST st_rep) -> case runRW# st_rep of (# _, a #) -> map (\x -> case x of { Just y -> y; ~rr -> def }) a)
   (lookupLSized n pairs keys)
{-# INLINE getBFstLLSized' #-}

getBFstLArrSized' :: (Hashable k, Ix i, Eq k) => Int -> b -> [(k, b)] -> Array i k -> Array i b
getBFstLArrSized' n def pairs keysArr =
 (\(ST st_rep) -> case runRW# st_rep of (# _, a #) -> amap (\x -> case x of { Just y -> y; ~rr -> def }) a)
   (lookupLSized n pairs keysArr)
{-# INLINE getBFstLArrSized' #-}
