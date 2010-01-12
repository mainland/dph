{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module DArray ( 
    DArray (..)
  , toDArray 
  , toScalar
  , fromDArray
  , forceDArray
  , backpermute
  , backpermuteDft
  , map
  , zip
  , zipWith
  , fold
  , mapFold
  , mapStencil
  , shift
  , reshape
  , rotate
  , tile
  , append
  , select
  , replicate
  , index
  , splitDArray
  , joinDArray
  ) where
         
import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Unlifted ((:*:)(..))
import Data.Array.Parallel.Base (Rebox)

import Data.Array.Parallel.Unlifted.Gabi (mapU,foldU,enumFromToU)

import qualified Array as A
import Prelude hiding (map, zip, zipWith, replicate)

import Debug.Trace

data DArray dim e where 
  DArray :: dim -> (dim -> e) -> DArray dim e

instance (U.Elt e, A.Shape dim, Show e) => Show (DArray dim e) where
  show darr = show $ fromDArray darr

assert a b = b


--  Constructors
--  ============

-- |Convert a strict array into a delayed array
toDArray:: (U.Elt e, A.Shape dim) => A.Array dim e -> DArray dim e
{-# INLINE toDArray #-}
toDArray arr = A.arrayShape arr `seq` A.arrayData arr `seq`
  DArray (A.arrayShape arr) 
         (\i -> ((A.arrayData arr) U.!: (A.toIndex (A.arrayShape arr) i)))

-- |Convert a zer dimensional array into a scalar value
toScalar :: U.Elt e => DArray () e -> e
toScalar (DArray _ fn) = fn ()

-- |Convert delayed array into strict array, force evaluation
fromDArray:: (U.Elt e, A.Shape dim) => DArray dim e -> A.Array dim e
{-# INLINE fromDArray #-}
fromDArray (DArray shape fn)
   = A.Array { A.arrayData = 
                     -- relative performance of these implementations differs
                     -- depending on ghc optimisations (fusion, inline patch)
                     U.map (fn . i) (U.enumFromTo (0::Int) ((A.size shape) - 1))  
                     -- U.mapRange fn A.next A.zeroDim shape (A.size shape)
                     -- U.map fn (A.range shape)
             , A.arrayShape = shape}
     where
       i = A.fromIndex shape

-- |Makes sure the underlying Unboxed.Array is evaluated, and the DArray function
-- is a simple look up
forceDArray:: (U.Elt e, A.Shape dim) => DArray dim e -> DArray dim e
{-# INLINE forceDArray #-}
forceDArray arr@(DArray d _) = A.arrayData arr' `seq` (toDArray arr')
      where  
        arr' = fromDArray arr


--  Basic operations
--  =================

-- |Generalised array backpermutation: arguments: delayed array, a target shape
--  and a function mapping each index in the target shape range to an index 
--  of the src array range.
backpermute:: (U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> dim' -> (dim' -> dim) -> DArray dim' e
{-# INLINE backpermute #-}
backpermute (DArray shape fn) newSh fn' =
  DArray newSh (fn.fn') 

backpermuteDft::(U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> e -> dim' -> (dim' -> Maybe dim) -> DArray dim' e
{-# INLINE backpermuteDft #-}
backpermuteDft srcArr@(DArray sh fn) e newSh fn' = e `seq` 
  DArray newSh fn''
  where
    fn'' i = case (fn' i) of
               Just i' -> fn i'
               Nothing -> e  

--  Computations
--  ============

-- | Map function over each element of N-dim DArray
map:: (U.Elt a, U.Elt b, A.Shape dim) => 
  (a -> b) -> DArray dim a -> DArray dim b
{-# INLINE map #-}
map fn' (DArray shape fn) = 
  DArray shape (fn'.fn)

-- | If the size of two array arguments differ in a dimension, the resulting
--   array's shape is the minimum of the two 
zipWith:: (U.Elt a, U.Elt b, U.Elt c, A.Shape dim) => 
  (a -> b -> c) -> DArray dim a -> DArray dim b-> DArray dim c
{-# INLINE zipWith #-}
zipWith f (DArray shape1 fn1) (DArray shape2 fn2) = 
  DArray (A.intersectDim shape1 shape2) (\i -> f (fn1 i) (fn2 i))



-- | If the size of two array arguments differ in a dimension, the resulting
--   array's shape is the minimum of the two 
zip:: (U.Elt a, U.Elt b, A.Shape dim) => 
  DArray dim a -> DArray dim b-> DArray dim (a :*: b)
{-# INLINE zip #-}
zip (DArray shape1 fn1) (DArray shape2 fn2) = 
  DArray (A.intersectDim shape1 shape2) (\i -> (fn1 i) :*: (fn2 i))
         

{-
fold :: (U.Elt e, A.Shape dim) => (e -> e-> e) -> e -> DArray dim e  -> e
{-# INLINE fold #-}
fold f n arr = 
  A.fold f n  $ fromDArray arr
-}
-- | folds the innermost dimension. Combine with `transpose to fold any other dimension.
fold :: (U.Elt e, A.Shape dim) => 
 (e -> e-> e) -> e -> DArray (dim :*: Int)  e  -> DArray dim e
{-# INLINE fold #-}
fold f n arr@(DArray sh@(sh' :*: segSize) fn) = toDArray $ A.mapFold f n $ fromDArray arr



-- | folds the innermost dimension. Combine with `transpose to fold any other dimension.
mapFold:: (U.Elt e, A.Shape dim) => (e -> e-> e) -> e -> DArray (dim :*: Int) e  -> DArray dim  e
{-# INLINE mapFold #-}
mapFold f n arr@(DArray sh@(sh' :*: s) fn) = 
  DArray sh' f'
  where
    f' i = foldU f n (mapU (\s -> fn (i:*:s)) (enumFromToU 0 (s-1)))

-- | mapStencil isBorder stencilShape proj broderFn stencilFn arr: 
--    works only in the sequential version at the moment
mapStencil:: (A.Shape dim, A.Shape dim', U.Elt e) =>
   (dim -> Bool) -> dim' -> (dim -> dim' -> dim) -> (e -> e') -> (DArray dim' e -> e') -> DArray dim e -> DArray dim e'
{-# INLINE mapStencil #-}
mapStencil border stencilSize stencil g f arr@(DArray sh arrFn) =
  DArray sh resFn
  where
    resFn d = 
      if (border d)
        then g $ arrFn d
        else let df' = \d' -> arrFn (stencil d  d')
             in f (DArray stencilSize df')           


----  Non-primitive functions 
----  ========================

shift:: (A.Subshape dim dim', U.Elt e) => DArray dim e -> e -> dim' -> DArray dim e
{-# INLINE shift #-}
shift arr@(DArray sh _) e shiftOffset = backpermuteDft arr  e sh
  (\d -> if (A.inRange sh (A.addDim d shiftOffset)) 
           then Just (A.addDim d shiftOffset) 
           else Nothing)

reshape:: (A.Shape dim, A.Shape dim', U.Elt e) => DArray dim e -> dim' -> DArray dim' e
{-# INLINE reshape #-}
reshape arr@(DArray sh fn) newShape = assert (A.size newShape == A.size sh) $
  DArray newShape (fn .  (A.fromIndex sh). (A.toIndex newShape))






rotate:: (A.Subshape dim dim', U.Elt e) => DArray dim e -> e -> dim' -> DArray dim e
{-# INLINE rotate #-}
rotate arr@(DArray sh _) e shiftOffset = backpermute arr  sh
  (\d -> A.addModDim sh d shiftOffset)


-- todo: generalise
tile::  (A.Subshape dim dim', U.Elt e) => DArray dim e -> dim' -> dim' -> DArray dim e
{-# INLINE tile #-}
tile arr@(DArray sh _) start size = 
--  assert (A.inRange sh (A.addDim start size)) $
     backpermute arr (A.inject sh size)
     (\d -> A.addDim d start)


--  Combining arrays
-- 

--   

append:: (A.Subshape dim dim, U.Elt e) => DArray dim e -> DArray dim e -> dim -> DArray dim e
{-# INLINE append #-}
append arr1@(DArray sh1 fn1) arr2@(DArray sh2 fn2) newSh =
  DArray newSh appFn
  where
    appFn i = if (A.inRange sh1 i) 
                then fn1 i
                else fn2 (A.modDim i sh1)
  

--  Shape polymorphic ops based on Index
--  ====================================


select:: (U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> A.SelectIndex dim dim'  -> DArray dim' e
{-# INLINE select #-}
select arr@(DArray shape _ ) ind = 
  backpermute arr (A.projShape ind shape) (selectFun ind)
  where
    selectFun:: A.SelectIndex dim1 dim2 -> dim2 -> dim1
    selectFun A.IndexNil sh = sh
    selectFun (A.IndexAll rsh) (shs :*: s) = (selectFun rsh shs) :*: s
    selectFun (A.IndexFixed n rsh) shs     = (selectFun rsh shs) :*: n




replicate:: (U.Elt e, A.Shape dim, A.Shape dim', A.InitShape dim, A.RepFun dim) => 
  DArray dim' e -> A.SelectIndex dim dim'  -> DArray dim e
{-# INLINE replicate #-}
replicate arr@(DArray shape _ ) ind = 
  backpermute arr (A.initShape ind shape) (A.repFun ind)



index::(U.Elt e, A.Shape dim) => DArray dim e -> dim -> e
{-# INLINE index #-}
index arr@(DArray _ fn) i =
    fn i  


--  Split and Joins should result in irregular arrays with regular element type. For now, though, 
--  it's implemented using lists

splitDArray:: (U.Elt e, A.Shape dim) => 
  DArray dim e -> A.MapIndex dim dim' ->  [DArray dim' e]
splitDArray _ =
  error "splitDArray: not yet implemented"

joinDArray:: (U.Elt e, A.Shape dim) => [DArray dim e] -> DArray (dim :*: Int) e 
joinDArray _ = 
  error "joinDArray: not yet implemented"



-- Class instances for arithmetic operations
-- =========================================

instance (U.Elt e, Eq e, A.Shape sh) => Eq (DArray sh e) where
  (==) arr1@(DArray sh _)  arr2 = 
    toScalar $ mapFold (&&) True $ (flip reshape) (() :*: (A.size sh)) $ zipWith (==) arr1 arr2
  (/=) a1 a2 = not $ (==) a1 a2

instance (U.Elt e, Num e, A.Shape sh) => Num (DArray sh e) where
  (+) = zipWith (+) 
  (*) = zipWith (*)
  (-) = zipWith (-)
  negate = map negate
  abs    = map abs
  signum = map signum
  fromInteger n = DArray A.zeroDim (\_ -> fromInteger n)

