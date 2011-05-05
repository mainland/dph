{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Scalar
where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Unboxed
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.PArray.PReprInstances

import qualified Data.Array.Parallel.Unlifted as U

import Data.Array.Parallel.Base (fromBool, toBool)

import GHC.Exts ( Int(..), (-#) )
import GHC.Word ( Word8 )

fromUArrPA :: Scalar a => Int -> U.Array a -> PArray a
{-# INLINE fromUArrPA #-}
fromUArrPA (I# n#) xs = PArray n# (toScalarPData xs)

toUArrPA :: Scalar a => PArray a -> U.Array a
{-# INLINE toUArrPA #-}
toUArrPA (PArray _ xs) = fromScalarPData xs

prim_lengthPA :: Scalar a => PArray a -> Int
{-# INLINE prim_lengthPA #-}
prim_lengthPA xs = I# (lengthPA# xs)

fromUArrPA' :: Scalar a => U.Array a -> PArray a
{-# INLINE fromUArrPA' #-}
fromUArrPA' xs = fromUArrPA (U.length xs) xs

scalar_map :: (Scalar a, Scalar b) => (a -> b) -> PArray a -> PArray b
{-# INLINE_PA scalar_map #-}
scalar_map f xs = fromUArrPA (prim_lengthPA xs)
                . U.map f
                $ toUArrPA xs

scalar_zipWith :: (Scalar a, Scalar b, Scalar c)
               => (a -> b -> c) -> PArray a -> PArray b -> PArray c
{-# INLINE_PA scalar_zipWith #-}
scalar_zipWith f xs ys = fromUArrPA (prim_lengthPA xs)
                       $ U.zipWith f (toUArrPA xs) (toUArrPA ys)

scalar_zipWith3
  :: (Scalar a, Scalar b, Scalar c, Scalar d)
  => (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d
{-# INLINE_PA scalar_zipWith3 #-}
scalar_zipWith3 f xs ys zs
  = fromUArrPA (prim_lengthPA xs)
  $ U.zipWith3 f (toUArrPA xs) (toUArrPA ys) (toUArrPA zs)

scalar_fold :: Scalar a => (a -> a -> a) -> a -> PArray a -> a
{-# INLINE_PA scalar_fold #-}
scalar_fold f z = U.fold f z . toUArrPA

scalar_fold1 :: Scalar a => (a -> a -> a) -> PArray a -> a
{-# INLINE_PA scalar_fold1 #-}
scalar_fold1 f = U.fold1 f . toUArrPA

scalar_folds :: Scalar a => (a -> a -> a) -> a -> PArray (PArray a) -> PArray a
{-# INLINE_PA scalar_folds #-}
scalar_folds f z xss = fromUArrPA (prim_lengthPA (concatPA# xss))
                     . U.fold_s f z (segdPA# xss)
                     . toUArrPA
                     $ concatPA# xss

scalar_fold1s :: Scalar a => (a -> a -> a) -> PArray (PArray a) -> PArray a
{-# INLINE_PA scalar_fold1s #-}
scalar_fold1s f xss = fromUArrPA (prim_lengthPA (concatPA# xss))
                    . U.fold1_s f (segdPA# xss)
                    . toUArrPA
                    $ concatPA# xss

scalar_fold1Index :: Scalar a
                  => ((Int, a) -> (Int, a) -> (Int, a)) -> PArray a -> Int
{-# INLINE_PA scalar_fold1Index #-}
scalar_fold1Index f = fst . U.fold1 f . U.indexed . toUArrPA

scalar_fold1sIndex :: Scalar a
                   => ((Int, a) -> (Int, a) -> (Int, a))
                   -> PArray (PArray a) -> PArray Int
{-# INLINE_PA scalar_fold1sIndex #-}
scalar_fold1sIndex f (PArray m# (PNested segd xs))
  = PArray m#
  $ toScalarPData
  $ U.fsts
  $ U.fold1_s f segd
  $ U.zip (U.indices_s segd)
  $ fromScalarPData xs

{-
scalar_fold1sIndex f xss = fromUArrPA n
                         . U.fsts
                         . U.fold1_s f' segd
                         . U.zip (U.indices_s m segd n)
                         . toUArrPA
                         $ concatPA# xss
  where
    {-# INLINE f' #-}
    f' p q = pairS $ f (unpairS p) (unpairS q)

    m = I# (lengthPA# xss)
    n = I# (lengthPA# (concatPA# xss))

    segd = segdPA# xss
-}

-- FIXME: hack!
--
instance Scalar Bool where
  {-# INLINE toScalarPData #-}
  toScalarPData bs
    = PBool (U.tagsToSel2 (U.map fromBool bs))

  {-# INLINE fromScalarPData #-}
  fromScalarPData (PBool sel) = U.map toBool (U.tagsSel2 sel)

fromUArrPA_2 :: (Scalar a, Scalar b) => Int -> U.Array (a,b) -> PArray (a,b)
{-# INLINE fromUArrPA_2 #-}
fromUArrPA_2 (I# n#) ps = PArray n# (P_2 (toScalarPData xs) (toScalarPData  ys))
  where
    (xs,ys) = U.unzip ps

fromUArrPA_2' :: (Scalar a, Scalar b) => U.Array (a,b) -> PArray (a, b)
{-# INLINE fromUArrPA_2' #-}
fromUArrPA_2' ps = fromUArrPA_2 (U.length ps) ps

fromUArrPA_3 :: (Scalar a, Scalar b, Scalar c)
             => Int -> U.Array ((a,b),c) -> PArray (a,b,c)
{-# INLINE fromUArrPA_3 #-}
fromUArrPA_3 (I# n#) ps = PArray n# (P_3 (toScalarPData xs)
                                         (toScalarPData ys)
                                         (toScalarPData zs))
  where
    (qs,zs) = U.unzip ps
    (xs,ys) = U.unzip qs

fromUArrPA_3' :: (Scalar a, Scalar b, Scalar c) => U.Array ((a,b),c) -> PArray (a, b, c)
{-# INLINE fromUArrPA_3' #-}
fromUArrPA_3' ps = fromUArrPA_3 (U.length ps) ps

nestUSegdPA :: Int -> U.Segd -> PArray a -> PArray (PArray a)
{-# INLINE nestUSegdPA #-}
nestUSegdPA (I# n#) segd (PArray _ xs) = PArray n# (PNested segd xs)

nestUSegdPA' :: U.Segd -> PArray a -> PArray (PArray a)
{-# INLINE nestUSegdPA' #-}
nestUSegdPA' segd xs = nestUSegdPA (U.lengthSegd segd) segd xs

