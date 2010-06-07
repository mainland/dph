-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.USel
-- Copyright   :  (c) 2010         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Selectors
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Flat.USel (

  -- * Types
  USel2,

  -- * Operations on selectors
  lengthUSel2, tagsUSel2, indicesUSel2, elementsUSel2_0, elementsUSel2_1,
  mkUSel2
) where

import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UArr, lengthU )

data USel2 = USel2 { usel2_tags      :: !(UArr Int)
                   , usel2_indices   :: !(UArr Int)
                   , usel2_elements0 :: !Int
                   , usel2_elements1 :: !Int
                   }

lengthUSel2 :: USel2 -> Int
{-# INLINE lengthUSel2 #-}
lengthUSel2 = lengthU . usel2_tags

tagsUSel2 :: USel2 -> UArr Int
{-# INLINE tagsUSel2 #-}
tagsUSel2 = usel2_tags

indicesUSel2 :: USel2 -> UArr Int
{-# INLINE indicesUSel2 #-}
indicesUSel2 = usel2_indices

elementsUSel2_0 :: USel2 -> Int
{-# INLINE elementsUSel2_0 #-}
elementsUSel2_0 = usel2_elements0

elementsUSel2_1 :: USel2 -> Int
{-# INLINE elementsUSel2_1 #-}
elementsUSel2_1 = usel2_elements1

mkUSel2 :: UArr Int -> UArr Int -> Int -> Int -> USel2
{-# INLINE mkUSel2 #-}
mkUSel2 = USel2
