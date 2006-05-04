-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Listlike
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Unlifted segmented array versions of Nesl-like combinators.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.NeslLike (
  flattenSU, (>:), segmentU, toSU, bpermuteSU, {- crossU -}
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS)
import Data.Array.Parallel.Arr.BUArr (
  indexBU)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Flat.ListLike
  ((!:))
import Data.Array.Parallel.Unlifted.Flat.NeslLike (
  toU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr, toUSegd, (>:), flattenSU, psumUS) 
import Data.Array.Parallel.Unlifted.Segmented.Loop (
  loopSU)
import Data.Array.Parallel.Unlifted.Segmented.Fusion (
  loopArrS)

-- |Segmentation
-- -------------

-- |Segment an array according to the segmentation of the first argument
--
segmentU :: (UA e', UA e) => SUArr e' -> UArr e -> SUArr e
{-# INLINE segmentU #-}
segmentU template arr = fstS (flattenSU template) >: arr


-- |Conversion
-- -----------

-- |Turn a nested list into a segmented parallel array
--
toSU :: UA e => [[e]] -> SUArr e
{-# INLINE toSU #-}
toSU ls = let lens = toU $ map length ls
	      a    = toU $ concat ls
          in
	  toUSegd lens >: a

-- |Permutations
-- -------------

-- |Segmented back permute
--
bpermuteSU :: UA e => SUArr e -> SUArr Int -> SUArr e
{-# INLINE bpermuteSU #-}
bpermuteSU as = loopArrS . loopSU extract nextOff 0
	        where
		  (segd :*: a) = flattenSU as
		  psum	       = psumUS segd
		  --
	          extract off i = (off :*: (Just $ a!:(off + i)))
		  --
		  nextOff _ segi = (psum `indexBU` (segi + 1) :*: 
				    (Nothing::Maybe ()))
