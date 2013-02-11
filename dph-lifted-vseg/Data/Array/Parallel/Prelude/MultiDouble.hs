{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.MultiDouble 
        ( Double
        
          -- * Ord
        , (==), (/=), (<), (<=), (>), (>=), min, max
        , maximumP,  minimumP
        , maxIndexP, minIndexP

          -- * Num
        , (+), (-), (*), (/)
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
-- Primitives needed by the vectoriser.
import Data.Array.Parallel.Prim                 ()      
import Data.Array.Parallel.Prelude.Base         (Bool)
import Data.Array.Parallel.PArr
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Lifted                       ((:->)(..))
import qualified Data.Array.Parallel.Lifted             as L
import qualified Data.Array.Parallel.PArray.Scalar      as SC
import qualified Prelude as P
import Prelude                                          (Int, Double)


infixl 7 *, /
infixl 6 +, -
infix  4 ==, /=, <, <=, >, >=

-- Ord ------------------------------------------------------------------------
(==), (/=), (<), (<=), (>), (>=) :: Double -> Double -> Bool
(==) = (P.==)
(/=) = (P./=)
(<=) = (P.<=)
(<)  = (P.<)
(>=) = (P.>=)
(>)  = (P.>)

-- min/max ----------------------------
min, max :: Double -> Double -> Double
min = P.min
max = P.max

-- minimum/maximum --------------------
minimumP, maximumP :: PArr Double -> Double

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
minIndexP :: PArr Double -> Int
minIndexP !_    = 0 
{-# NOINLINE  minIndexP #-}
{-# VECTORISE minIndexP = minIndexPP #-}

minIndexPP :: PArray Double :-> Int
minIndexPP      = L.closure1' (SC.fold1Index min') (SC.fold1sIndex min')
{-# INLINE      minIndexPP #-}
{-# NOVECTORISE minIndexPP #-}

min' :: P.Ord b => (a, b) -> (a, b) -> (a, b)
min' (i,x) (j,y) | x P.<= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE min' #-}


maxIndexP :: PArr Double -> Int
maxIndexP _     = 0
{-# NOINLINE  maxIndexP #-}
{-# VECTORISE maxIndexP = maxIndexPP #-}

maxIndexPP :: PArray Double :-> Int
maxIndexPP      = L.closure1' (SC.fold1Index max') (SC.fold1sIndex max')
{-# INLINE      maxIndexPP #-}
{-# NOVECTORISE maxIndexPP #-}

max' :: P.Ord b => (a, b) -> (a, b) -> (a, b)
max' (i,x) (j,y) | x P.>= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE max' #-}


-- Num ---------------------------------------------------------------------
(+), (-), (*), (/) :: Double -> Double -> Double
(+) = (P.+)
(-) = (P.-)
(*) = (P.*)
(/) = (P./)

#if defined(__GLASGOW_HASKELL_LLVM__)
{-# NOINLINE  (+) #-}
{-# VECTORISE (+) = addPP #-}

addPP :: Double :-> Double :-> Double
addPP = L.closure2' (P.+) (SC.mzipWith (P.+) (P.+))
{-# INLINE addPP #-}
{-# NOVECTORISE addPP #-}

{-# NOINLINE  (-) #-}
{-# VECTORISE (-) = subPP #-}

subPP :: Double :-> Double :-> Double
subPP = L.closure2' (P.-) (SC.mzipWith (P.-) (P.-))
{-# INLINE subPP #-}
{-# NOVECTORISE subPP #-}

{-# NOINLINE  (*) #-}
{-# VECTORISE (*) = mulPP #-}

mulPP :: Double :-> Double :-> Double
mulPP   = L.closure2' (P.*) (SC.mzipWith (P.*) (P.*))
{-# INLINE mulPP #-}
{-# NOVECTORISE mulPP #-}
#endif /* !defined(__GLASGOW_HASKELL_LLVM__) */

-- negate/abs -------------------------
negate, abs :: Double -> Double
negate  = P.negate
abs     = P.abs

-- sum/product ------------------------
sumP, productP :: PArr Double -> Double

sumP arr        = headPArr arr
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP      = sumPP #-}

productP arr    = headPArr arr
{-# NOINLINE  productP #-}
{-# VECTORISE productP  = productPP #-}

sumPP, productPP :: PArray Double :-> Double
#if defined(__GLASGOW_HASKELL_LLVM__)
sumPP          = L.closure1' (SC.mfold (+) (P.+) 0) (SC.folds (+) 0)
#else /* !defined(__GLASGOW_HASKELL_LLVM__) */
sumPP          = L.closure1' (SC.fold (+) 0) (SC.folds (+) 0)
#endif /* !defined(__GLASGOW_HASKELL_LLVM__) */
{-# INLINE      sumPP #-}
{-# NOVECTORISE sumPP #-}

#if defined(__GLASGOW_HASKELL_LLVM__)
productPP      = L.closure1' (SC.mfold (*) (P.*) 1) (SC.folds (*) 1)
#else /* !defined(__GLASGOW_HASKELL_LLVM__) */
productPP      = L.closure1' (SC.fold (*) 1) (SC.folds (*) 1)
#endif /* !defined(__GLASGOW_HASKELL_LLVM__) */
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
sqrt = P.sqrt
log = P.log
sin = P.sin
tan = P.tan
cos = P.cos
asin = P.asin
atan = P.atan
acos = P.acos
sinh = P.sinh
tanh = P.tanh
cosh = P.cosh
asinh = P.asinh
atanh = P.atanh
acosh = P.acosh

(**), logBase :: Double -> Double -> Double
(**)    = (P.**)
logBase = P.logBase

-- RealFrac -------------------------------------------------------------------
fromInt :: Int -> Double
fromInt = P.fromIntegral
