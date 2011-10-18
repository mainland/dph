{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Double 
        ( Double
        
        -- * Ord
        , (==), (/=), (<), (<=), (>), (>=), min, max
        , maximumP,  minimumP
        , maxIndexP, minIndexP

        -- * Num
        , (+), (-), (*)
        , negate, abs
        , sumP, productP
        
        -- * Floating
        , pi
        , sqrt
        , exp, (**)
        , log, logBase
        ,  sin,   tan,   cos
        , asin,  atan,  acos
        ,  sinh,  tanh,  cosh
        , asinh, atanh, acosh
        
        -- * RealFrac
        , fromInt)
where
import Data.Array.Parallel.VectDepend
-- IMPORTANT: see Note [Vectoriser dependencies] in the same module

import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Int                  (Int)
import Data.Array.Parallel.PArr
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Lifted                       ((:->)(..))
import qualified Data.Array.Parallel.Lifted             as L
import qualified Data.Array.Parallel.PArray.Scalar      as SC
import qualified Prelude as P
import Prelude (Double)
        
{-# VECTORISE SCALAR type Double #-}

-- Ord ------------------------------------------------------------------------
(==), (/=), (<), (<=), (>), (>=) :: Double -> Double -> Bool

(==) = (P.==)
{-# VECTORISE SCALAR (==) #-}

(/=) = (P./=)
{-# VECTORISE SCALAR (/=) #-}

(<=) = (P.<=)
{-# VECTORISE SCALAR (<=) #-}

(<)  = (P.<)
{-# VECTORISE SCALAR (<) #-}

(>=) = (P.>=)
{-# VECTORISE SCALAR (>=) #-}

(>)  = (P.>)
{-# VECTORISE SCALAR (>) #-}


-- min/max ----------------------------
min, max :: Double -> Double -> Double

min = P.min
{-# VECTORISE SCALAR min #-}

max = P.max
{-# VECTORISE SCALAR max #-}


-- minimum/maximum --------------------
minimumP, maximumP :: [:Double:] -> Double

minimumP arr    = headPArr arr
{-# NOINLINE  minimumP #-}
{-# VECTORISE minimumP = minimumPP #-}

maximumP arr    = headPArr arr
{-# NOINLINE  maximumP #-}
{-# VECTORISE maximumP = maximumPP #-}

minimumPP, maximumPP :: PArray Double :-> Double
minimumPP      = L.closure1' (SC.fold1 P.min) (SC.fold1s P.min)
{-# INLINE      minimumPP #-}
{-# NOVECTORISE minimumPP #-}

maximumPP      = L.closure1' (SC.fold1 P.max) (SC.fold1s P.max)
{-# INLINE      maximumPP #-}
{-# NOVECTORISE maximumPP #-}


-- minIndex/maxIndex ------------------
minIndexP :: [:Double:] -> Int
minIndexP !_    = 0 
{-# NOINLINE  minIndexP #-}
{-# VECTORISE minIndexP = minIndexPP #-}

minIndexPP :: PArray Double :-> Int
minIndexPP      = L.closure1' (SC.fold1Index min') (SC.fold1sIndex min')
{-# INLINE      minIndexPP #-}
{-# NOVECTORISE minIndexPP #-}

min' (i,x) (j,y) | x P.<= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE min' #-}


maxIndexP :: [:Double:] -> Int
maxIndexP _     = 0
{-# NOINLINE  maxIndexP #-}
{-# VECTORISE maxIndexP = maxIndexPP #-}

maxIndexPP :: PArray Double :-> Int
maxIndexPP      = L.closure1' (SC.fold1Index max') (SC.fold1sIndex max')
{-# INLINE      maxIndexPP #-}
{-# NOVECTORISE maxIndexPP #-}

max' (i,x) (j,y) | x P.>= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE max' #-}


-- Num ---------------------------------------------------------------------
(+), (-), (*) :: Double -> Double -> Double

(+) = (P.+)
{-# VECTORISE SCALAR (+) #-}

(-) = (P.-)
{-# VECTORISE SCALAR (-) #-}

(*) = (P.*)
{-# VECTORISE SCALAR (*) #-}


-- negate/abs -------------------------
negate, abs :: Double -> Double

negate  = P.negate
{-# VECTORISE SCALAR negate #-}

abs     = P.abs
{-# VECTORISE SCALAR abs #-}


-- sum/product ------------------------
sumP, productP :: [:Double:] -> Double

sumP arr        = headPArr arr
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP      = sumPP #-}

productP arr    = headPArr arr
{-# NOINLINE  productP #-}
{-# VECTORISE productP  = productPP #-}

sumPP, productPP :: PArray Double :-> Double
sumPP          = L.closure1' (SC.fold (+) 0) (SC.folds (+) 0)
{-# INLINE      sumPP #-}
{-# NOVECTORISE sumPP #-}

productPP      = L.closure1' (SC.fold (*) 1) (SC.folds (*) 1)
{-# INLINE      productPP #-}
{-# NOVECTORISE productPP #-}


-- Floating -------------------------------------------------------------------
pi :: Double
pi = P.pi
{-# NOVECTORISE pi #-}

sqrt,    exp,  log, 
  sin,   tan,   cos, 
 asin,  atan,  acos, 
  sinh,  tanh,  cosh,
 asinh, atanh, acosh  :: Double -> Double

exp = P.exp
{-# VECTORISE SCALAR exp #-}

sqrt = P.sqrt
{-# VECTORISE SCALAR sqrt #-}

log = P.log
{-# VECTORISE SCALAR log #-}

sin = P.sin
{-# VECTORISE SCALAR sin #-}

tan = P.tan
{-# VECTORISE SCALAR tan #-}

cos = P.cos
{-# VECTORISE SCALAR cos #-}

asin = P.asin
{-# VECTORISE SCALAR asin #-}

atan = P.atan
{-# VECTORISE SCALAR atan #-}

acos = P.acos
{-# VECTORISE SCALAR acos #-}

sinh = P.sinh
{-# VECTORISE SCALAR sinh #-}

tanh = P.tanh
{-# VECTORISE SCALAR tanh #-}

cosh = P.cosh
{-# VECTORISE SCALAR cosh #-}

asinh = P.asinh
{-# VECTORISE SCALAR asinh #-}

atanh = P.atanh
{-# VECTORISE SCALAR atanh #-}

acosh = P.acosh
{-# VECTORISE SCALAR acosh #-}


(**), logBase :: Double -> Double -> Double

(**)    = (P.**)
{-# VECTORISE SCALAR (**) #-}

logBase = P.logBase
{-# VECTORISE SCALAR logBase #-}


-- RealFrac -------------------------------------------------------------------
fromInt :: Int -> Double
fromInt         = P.fromIntegral
{-# VECTORISE SCALAR fromInt #-}