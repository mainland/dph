#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Void 
        (Void, void, pvoid, fromVoid, pvoids)
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.Pretty
import qualified Data.Vector            as V

-------------------------------------------------------------------------------
-- | The Void type is used as a place holder in situations where we don't 
--   want to track a real array.
--  
--   For example:
--    A type like Bool is represented as @Sum2 Void Void@, meaning that we only
--    only care about the tag of the data constructor and not its argumnent.
--
--    We also use it as the to fill empty closures.
--
--   Note that arrays of (PData Void) do not have an intrinsic length, which 
--   is the reason that the PR dictionary only contains a coversPR function
--   was well as a partial lengthPR function.
--
data instance PData Void

-- | PVoids instance counts how many "vectors" of void we have
data instance PDatas Void
        = PVoids Int

pvoid :: PData Void
pvoid   = error "Data.Array.Parallel.PData.Void"

pvoids :: Int -> PDatas Void
pvoids   = PVoids


-- PR --------------------------------------------------------------------------
nope str    = error $ "Data.Array.Parallel.PData.Void: no PR method for " ++ str

instance PR Void where

  {-# INLINE_PDATA validPR #-}
  validPR _       = True

  {-# INLINE_PDATA nfPR #-}
  nfPR _          = ()

  {-# INLINE_PDATA similarPR #-}
  similarPR _ _   = True
  
  {-# INLINE_PDATA coversPR #-}
  coversPR _ _ _  = True
  
  {-# INLINE_PDATA pprpDataPR #-}
  pprpDataPR _    = text "pvoid"


  -- Constructors -------------------------------        
  {-# INLINE_PDATA emptyPR #-}
  emptyPR       = nope "emptyPR"

  {-# INLINE_PDATA replicatePR #-}
  replicatePR   = nope "replicate"

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR  = nope "replicates"

  {-# INLINE_PDATA appendPR #-}
  appendPR      = nope "append"
  
  {-# INLINE_PDATA appendsPR #-}
  appendsPR     = nope "appends"


  -- Projections --------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR _    = nope "length"

  {-# INLINE_PDATA indexPR #-}
  indexPR       = nope "index"

  {-# INLINE_PDATA indexlPR #-}
  indexlPR      = nope "indexl"

  {-# INLINE_PDATA extractPR #-}
  extractPR     = nope "extractl"

  {-# INLINE_PDATA extractsPR #-}
  extractsPR    = nope "extracts"


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR   = nope "packByTag"

  {-# INLINE_PDATA combine2PR #-}
  combine2PR    = nope "combine2"


  -- Conversions --------------------------------
  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR  = nope "fromVector"

  -- This conversion is dodgy because it implies the array has length zero,
  -- where really should have "no length". This is ok if we're just using
  -- it for debugging.
  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR _  = V.empty


  -- PDatas -------------------------------------  
  {-# INLINE_PDATA emptydPR #-}    
  emptydPR      = PVoids 0

  {-# INLINE_PDATA singletondPR #-}    
  singletondPR _
        = PVoids 1

  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PVoids n)
        = n

  {-# INLINE_PDATA indexdPR #-}
  indexdPR _ _
        = pvoid
        
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PVoids n1) (PVoids n2)
        = PVoids (n1 + n2)

  {-# INLINE_PDATA concatdPR #-}
  concatdPR ps
        = PVoids $ sum [n | PVoids n <- V.toList ps]

  {-# INLINE_PDATA fromVectordPR #-}
  fromVectordPR vec
        = PVoids $ V.length vec

  {-# INLINE_PDATA toVectordPR #-}
  toVectordPR (PVoids n)
        = V.replicate n pvoid


-- Show -----------------------------------------------------------------------
instance Show (PData  Void) where
 show _  = "pvoid"


instance Show (PDatas Void) where
 show _  = "pvoids"
 

instance PprVirtual (PData Void) where
  pprv _ = text "pvoid"

