-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Combinators
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
--  Standard combinators for flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Combinators (
  mapU, filterU,
  foldlU, foldl1U, foldl1MaybeU, {-foldrU, foldr1U,-}
  foldU,  fold1U,  fold1MaybeU,
  scanlU, scanl1U, {-scanrU, scanr1U,-} scanU, scan1U,
  mapAccumLU,
  zipU, zip3U, unzipU, unzip3U, fstU, sndU,
  zipWithU, zipWith3U
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..), checkNotEmpty)
import Data.Array.Parallel.Stream (
  mapS, filterS, foldS, fold1MaybeS, scanS, mapAccumS,
  zipWithS, zipWith3S)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr,
  zipU, unzipU, fstU, sndU)
import Data.Array.Parallel.Unlifted.Flat.Stream (
  streamU, unstreamU)
import Data.Array.Parallel.Unlifted.Flat.Basics (
  lengthU, (!:))
import Data.Array.Parallel.Unlifted.Flat.Subarrays (
  sliceU)

here s = "Data.Array.Parallel.Unlifted.Flat.Combinators." ++ s

-- |Map a function over an array
--
mapU :: (UA e, UA e') => (e -> e') -> UArr e -> UArr e'
{-# INLINE mapU #-}
mapU f = unstreamU . mapS f . streamU

-- |Extract all elements from an array that meet the given predicate
--
filterU :: UA e => (e -> Bool) -> UArr e -> UArr e 
{-# INLINE filterU #-}
filterU p = unstreamU . filterS p . streamU

-- |Array reduction proceeding from the left
--
foldlU :: UA a => (b -> a -> b) -> b -> UArr a -> b
{-# INLINE foldlU #-}
foldlU f z = foldS f z . streamU

-- |Array reduction proceeding from the left for non-empty arrays
--
-- FIXME: Rewrite for 'Stream's.
--
foldl1U :: UA a => (a -> a -> a) -> UArr a -> a
{-# INLINE foldl1U #-}
foldl1U f arr = checkNotEmpty (here "foldl1U") (lengthU arr) $
                foldlU f (arr !: 0) (sliceU arr 1 (lengthU arr - 1))

foldl1MaybeU :: UA a => (a -> a -> a) -> UArr a -> MaybeS a
{-# INLINE foldl1MaybeU #-}
foldl1MaybeU f = fold1MaybeS f . streamU

-- |Array reduction that requires an associative combination function with its
-- unit
--
foldU :: UA a => (a -> a -> a) -> a -> UArr a -> a
{-# INLINE foldU #-}
foldU = foldlU

fold1MaybeU :: UA a => (a -> a -> a) -> UArr a -> MaybeS a
{-# INLINE fold1MaybeU #-}
fold1MaybeU = foldl1MaybeU

-- |Reduction of a non-empty array which requires an associative combination
-- function
--
fold1U :: UA a => (a -> a -> a) -> UArr a -> a
{-# INLINE fold1U #-}
fold1U = foldl1U

-- |Prefix scan proceedings from left to right
--
scanlU :: (UA a, UA b) => (b -> a -> b) -> b -> UArr a -> UArr b
{-# INLINE scanlU #-}
scanlU f z = unstreamU . scanS f z . streamU

-- |Prefix scan of a non-empty array proceeding from left to right
--
-- FIXME: Rewrite for 'Stream's.
--
scanl1U :: UA a => (a -> a -> a) -> UArr a -> UArr a
{-# INLINE scanl1U #-}
scanl1U f arr = checkNotEmpty (here "scanl1U") (lengthU arr) $
                scanlU f (arr !: 0) (sliceU arr 1 (lengthU arr - 1))

-- |Prefix scan proceedings from left to right that needs an associative
-- combination function with its unit
--
scanU :: UA a => (a -> a -> a) -> a -> UArr a -> UArr a
{-# INLINE scanU #-}
scanU = scanlU

-- |Prefix scan of a non-empty array proceedings from left to right that needs
-- an associative combination function
--
scan1U :: UA a => (a -> a -> a) -> UArr a -> UArr a
{-# INLINE scan1U #-}
scan1U = scanl1U

-- |Accumulating map from left to right. Does not return the accumulator.
--
-- FIXME: Naming inconsistent with lists.
--
mapAccumLU :: (UA a, UA b) => (c -> a -> c :*: b) -> c -> UArr a -> UArr b
{-# INLINE mapAccumLU #-}
mapAccumLU f z = unstreamU . mapAccumS f z . streamU

-- zipU is re-exported from UArr

-- |
--
zip3U :: (UA e1, UA e2, UA e3) 
      => UArr e1 -> UArr e2 -> UArr e3 -> UArr (e1 :*: e2 :*: e3)
{-# INLINE zip3U #-}
zip3U a1 a2 a3 = (a1 `zipU` a2) `zipU` a3

-- |
zipWithU :: (UA a, UA b, UA c) 
	 => (a -> b -> c) -> UArr a -> UArr b -> UArr c
{-# INLINE zipWithU #-}
zipWithU f a1 a2 = unstreamU (zipWithS f (streamU a1) (streamU a2))

-- |
zipWith3U :: (UA a, UA b, UA c, UA d) 
          => (a -> b -> c -> d) -> UArr a -> UArr b -> UArr c -> UArr d
{-# INLINE zipWith3U #-}
zipWith3U f a1 a2 a3 = unstreamU (zipWith3S f (streamU a1)
                                              (streamU a2)
                                              (streamU a3))

-- unzipU is re-exported from UArr

-- |
unzip3U :: (UA e1, UA e2, UA e3) 
	=> UArr (e1 :*: e2 :*: e3) -> (UArr e1 :*: UArr e2 :*: UArr e3)
{-# INLINE unzip3U #-}
unzip3U a = let (a12 :*: a3) = unzipU a
		(a1  :*: a2) = unzipU a12
	    in
	    (a1 :*: a2 :*: a3)

-- fstU and sndU reexported from UArr

