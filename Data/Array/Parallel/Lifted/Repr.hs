{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Repr (
  PArray(..),
  Void, void,
  Wrap(..),
  Enumeration(..),
  Sum2(..), Sum3(..), 

  dPA_Void,
  dPR_Void, dPR_Unit, dPR_Wrap,
  dPR_Enumeration,
  dPR_2, dPR_3, dPR_4, dPR_5, zipPA#, unzipPA#, zip3PA#,
  fromUArrPA_2, fromUArrPA_2',
  fromUArrPA_3, fromUArrPA_3',
  dPR_Sum2, dPR_Sum3,

  dPR_PArray, nested_lengthPA, concatPA#,
  toSUArrPA, fromSUArrPA, fromSUArrPA_2, fromSUArrPA', fromSUArrPA_2'
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Unboxed
import Data.Array.Parallel.Unlifted

import GHC.Exts  (Int#, Int(..), (+#), (-#), (*#))
import Debug.Trace



data Void

void :: Void
void = error "Data.Array.Parallel.void"

data instance PArray Void = PVoid Int#

dPR_Void :: PR Void
{-# INLINE dPR_Void #-}
dPR_Void = PR {
             lengthPR     = lengthPR_Void
           , emptyPR      = emptyPR_Void
           , replicatePR  = replicatePR_Void
           , replicatelPR = replicatelPR_Void
           , repeatPR     = repeatPR_Void
           , indexPR      = indexPR_Void
           , bpermutePR   = bpermutePR_Void
           , appPR        = appPR_Void
           , applPR       = applPR_Void
           , packPR       = packPR_Void
           , combine2PR   = combine2PR_Void
           }

{-# INLINE lengthPR_Void #-}
lengthPR_Void (PVoid n#) = n#

{-# INLINE emptyPR_Void #-}
emptyPR_Void = PVoid 0#

{-# INLINE replicatePR_Void #-}
replicatePR_Void n# _ = PVoid n#

{-# INLINE replicatelPR_Void #-}
replicatelPR_Void n# _ _ = PVoid n#

{-# INLINE repeatPR_Void #-}
repeatPR_Void n# (PVoid m#) = PVoid (n# *# m#)

indexPR_Void :: PArray Void -> Int# -> Void
{-# INLINE indexPR_Void #-}
indexPR_Void (PVoid n#) i# = void

{-# INLINE bpermutePR_Void #-}
bpermutePR_Void (PVoid _) is = PVoid (lengthPA_Int# is)

{-# INLINE appPR_Void #-}
appPR_Void (PVoid m#) (PVoid n#) = PVoid (m# +# n#)

{-# INLINE applPR_Void #-}
applPR_Void _ (PVoid m#) _ (PVoid n#) = PVoid (m# +# n#)

{-# INLINE packPR_Void #-}
packPR_Void (PVoid _) n# _ = PVoid n#

{-# INLINE combine2PR_Void #-}
combine2PR_Void n# _ _ (PVoid _) (PVoid _) = PVoid n#

type instance PRepr Void = Void

dPA_Void :: PA Void
{-# INLINE_PA dPA_Void #-}
dPA_Void = PA {
             toPRepr      = id
           , fromPRepr    = id
           , toArrPRepr   = id
           , fromArrPRepr = id
           , dictPRepr    = dPR_Void
           }

data instance PArray () = PUnit Int# ()

dPR_Unit :: PR ()
{-# INLINE dPR_Unit #-}
dPR_Unit = PR {
             lengthPR     = lengthPR_Unit
           , emptyPR      = emptyPR_Unit
           , replicatePR  = replicatePR_Unit
           , replicatelPR = replicatelPR_Unit
           , repeatPR     = repeatPR_Unit
           , indexPR      = indexPR_Unit
           , bpermutePR   = bpermutePR_Unit
           , appPR        = appPR_Unit
           , applPR       = applPR_Unit
           , packPR       = packPR_Unit
           , combine2PR   = combine2PR_Unit
           }
         

{-# INLINE lengthPR_Unit #-}
lengthPR_Unit (PUnit n# _) = n#

{-# INLINE emptyPR_Unit #-}
emptyPR_Unit = PUnit 0# ()

{-# INLINE replicatePR_Unit #-}
replicatePR_Unit n# u = PUnit n# u

{-# INLINE replicatelPR_Unit #-}
replicatelPR_Unit n# _ (PUnit _ u) = PUnit n# u

{-# INLINE repeatPR_Unit #-}
repeatPR_Unit n# (PUnit m# u) = PUnit (n# *# m#) u

indexPR_Unit :: PArray () -> Int# -> ()
{-# INLINE indexPR_Unit #-}
indexPR_Unit (PUnit n# u) i# = u

{-# INLINE bpermutePR_Unit #-}
bpermutePR_Unit (PUnit _ u) is = PUnit (lengthPA_Int# is) u

{-# INLINE appPR_Unit #-}
appPR_Unit (PUnit m# u) (PUnit n# v) = PUnit (m# +# n#) (u `seq` v)

{-# INLINE applPR_Unit #-}
applPR_Unit _ (PUnit m# u) _ (PUnit n# v) = PUnit (m# +# n#) (u `seq` v)

{-# INLINE packPR_Unit #-}
packPR_Unit (PUnit _ u) n# _ = PUnit n# u

{-# INLINE combine2PR_Unit #-}
combine2PR_Unit n# _ _ (PUnit _ u1) (PUnit _ u2)
  = PUnit n# (u1 `seq` u2)

data Wrap a = Wrap a

data instance PArray (Wrap a) = PWrap Int# (PArray a)

dPR_Wrap :: PR a -> PR (Wrap a)
{-# INLINE dPR_Wrap #-}
dPR_Wrap pr = PR {
              lengthPR     = lengthPR_Wrap
            , emptyPR      = emptyPR_Wrap pr
            , replicatePR  = replicatePR_Wrap pr
            , replicatelPR = replicatelPR_Wrap pr
            , repeatPR     = repeatPR_Wrap pr
            , indexPR      = indexPR_Wrap pr
            , bpermutePR   = bpermutePR_Wrap pr
            , appPR        = appPR_Wrap pr
            , applPR       = applPR_Wrap pr
            , packPR       = packPR_Wrap pr
            , combine2PR   = combine2PR_Wrap pr
            }

{-# INLINE lengthPR_Wrap #-}
lengthPR_Wrap (PWrap n# _) = n#

{-# INLINE emptyPR_Wrap #-}
emptyPR_Wrap pr = PWrap 0# (emptyPR pr)

{-# INLINE replicatePR_Wrap #-}
replicatePR_Wrap pr n# ~(Wrap x) = PWrap n# (replicatePR pr n# x)

{-# INLINE replicatelPR_Wrap #-}
replicatelPR_Wrap pr n# ns (PWrap _ xs) = PWrap n# (replicatelPR pr n# ns xs)

{-# INLINE repeatPR_Wrap #-}
repeatPR_Wrap pr n# (PWrap m# xs) = PWrap (n# *# m#) (repeatPR pr n# xs)

{-# INLINE indexPR_Wrap #-}
indexPR_Wrap pr (PWrap n# xs) i# = Wrap (indexPR pr xs i#)

{-# INLINE bpermutePR_Wrap #-}
bpermutePR_Wrap pr (PWrap n# xs) is = PWrap (lengthPA_Int# is)
                                            (bpermutePR pr xs is)

{-# INLINE appPR_Wrap #-}
appPR_Wrap pr (PWrap m# xs) (PWrap n# ys) = PWrap (m# +# n#) (appPR pr xs ys)

{-# INLINE applPR_Wrap #-}
applPR_Wrap pr is (PWrap m# xs) js (PWrap n# ys)
  = PWrap (m# +# n#) (applPR pr is xs js ys)

{-# INLINE packPR_Wrap #-}
packPR_Wrap pr (PWrap _ xs) n# sel# = PWrap n# (packPR pr xs n# sel#)

combine2PR_Wrap:: PR a -> Int# -> PArray_Int# -> PArray_Int#
                              -> PArray (Wrap a) -> PArray (Wrap a) -> PArray (Wrap a)
combine2PR_Wrap _ _ _ _ _ = error "combine2PR_Wrap nyi"

data Enumeration = Enumeration Int#

data instance PArray Enumeration = PEnum Int# PArray_Int# PArray_Int#

dPR_Enumeration :: PR Enumeration
{-# INLINE dPR_Enumeration #-}
dPR_Enumeration = PR {
                    lengthPR    = lengthPR_Enumeration
                  , emptyPR     = emptyPR_Enumeration
                  , replicatePR = replicatePR_Enumeration
                  }

{-# INLINE lengthPR_Enumeration #-}
lengthPR_Enumeration (PEnum n# _ _) = n#

{-# INLINE emptyPR_Enumeration #-}
emptyPR_Enumeration = PEnum 0# emptyPA_Int# emptyPA_Int#

{-# INLINE replicatePR_Enumeration #-}
replicatePR_Enumeration n# enum
  = PEnum n# (replicatePA_Int# n# (case enum of { Enumeration i# -> i# }))
             (upToPA_Int# n#)

data instance PArray (a,b)
  = P_2 Int# (PArray a)
             (PArray b)

data instance PArray (a,b,c)
  = P_3 Int# (PArray a)
             (PArray b)
             (PArray c)

data instance PArray (a,b,c,d)
  = P_4 Int# (PArray a)
             (PArray b)
             (PArray c)
             (PArray d)

data instance PArray (a,b,c,d,e)
  = P_5 Int# (PArray a)
             (PArray b)
             (PArray c)
             (PArray d)
             (PArray e)

fromUArrPA_2 :: (PrimPA a, PrimPA b) => Int -> UArr (a :*: b) -> PArray (a,b)
{-# INLINE fromUArrPA_2 #-}
fromUArrPA_2 (I# n#) ps = P_2 n# (fromUArrPA (I# n#) xs) (fromUArrPA (I# n#) ys)
  where
    xs :*: ys = unzipU ps



fromUArrPA_2' :: (PrimPA a, PrimPA b) => UArr (a :*: b) -> PArray (a, b)
{-# INLINE fromUArrPA_2' #-}
fromUArrPA_2' ps = fromUArrPA_2 (lengthU ps) ps

fromUArrPA_3 :: (PrimPA a, PrimPA b, PrimPA c) => Int -> UArr (a :*: b :*: c) -> PArray (a,b,c)
{-# INLINE fromUArrPA_3 #-}
fromUArrPA_3 (I# n#) ps = P_3 n# (fromUArrPA (I# n#) xs) (fromUArrPA (I# n#) ys) (fromUArrPA (I# n#) zs)
  where
    xs :*: ys :*: zs = unzip3U ps

fromUArrPA_3' :: (PrimPA a, PrimPA b, PrimPA c) => UArr (a :*: b :*: c) -> PArray (a, b, c)
{-# INLINE fromUArrPA_3' #-}
fromUArrPA_3' ps = fromUArrPA_3 (lengthU ps) ps


dPR_2 :: PR a -> PR b -> PR (a,b)
{-# INLINE dPR_2 #-}
dPR_2 pra prb
  = PR {
      lengthPR     = lengthPR_2
    , emptyPR      = emptyPR_2 pra prb
    , replicatePR  = replicatePR_2 pra prb
    , replicatelPR = replicatelPR_2 pra prb
    , repeatPR     = repeatPR_2 pra prb
    , indexPR      = indexPR_2 pra prb
    , bpermutePR   = bpermutePR_2 pra prb
    , appPR        = appPR_2 pra prb
    , applPR       = applPR_2 pra prb
    , packPR       = packPR_2 pra prb
    , combine2PR   = combine2PR_2 pra prb
    }

{-# INLINE lengthPR_2 #-}
lengthPR_2 (P_2 n# _ _) = n#

{-# INLINE emptyPR_2 #-}
emptyPR_2 pra prb = P_2 0# (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_2 #-}
replicatePR_2 pra prb n# ~(a,b)
  = P_2 n# (replicatePR pra n# a)
           (replicatePR prb n# b)

{-# INLINE replicatelPR_2 #-}
replicatelPR_2 pra prb n# ns (P_2 _ as bs)
  = P_2 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs) 

{-# INLINE repeatPR_2 #-}
repeatPR_2 pra prb n# (P_2 m# as bs)
  = P_2 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)

{-# INLINE indexPR_2 #-}
indexPR_2 pra prb (P_2 _ as bs) i# = (indexPR pra as i#, indexPR prb bs i#)

{-# INLINE bpermutePR_2 #-}
bpermutePR_2 pra prb (P_2 _ as bs) is
  = P_2 (lengthPA_Int# is) (bpermutePR pra as is)
                           (bpermutePR prb bs is)

{-# INLINE appPR_2 #-}
appPR_2 pra prb (P_2 m# as1 bs1) (P_2 n# as2 bs2)
  = P_2 (m# +# n#) (appPR pra as1 as2) (appPR prb bs1 bs2)

{-# INLINE applPR_2 #-}
applPR_2 pra prb is (P_2 m# as1 bs1) js (P_2 n# as2 bs2)
  = P_2 (m# +# n#) (applPR pra is as1 js as2)
                   (applPR prb is bs1 js bs2)

{-# INLINE packPR_2 #-}
packPR_2 pra prb (P_2 _ as bs) n# sel# = P_2 n# (packPR pra as n# sel#)
                                                (packPR prb bs n# sel#)

{-# INLINE combine2PR_2 #-}
combine2PR_2 pra prb n# sel# is# (P_2 _ as1 bs1) (P_2 _ as2 bs2)
  = P_2 n# (combine2PR pra n# sel# is# as1 as2)
           (combine2PR prb n# sel# is# bs1 bs2)

zipPA# :: PA a -> PA b -> PArray a -> PArray b -> PArray (a,b)
{-# INLINE_PA zipPA# #-}
zipPA# pa pb xs ys = P_2 (lengthPA# pa xs) xs ys

unzipPA# :: PA a -> PA b  -> PArray (a,b) -> (PArray a,  PArray b)
{-# INLINE_PA unzipPA# #-}
unzipPA# pa pb (P_2 n xs ys)  = (xs, ys)

dPR_3 :: PR a -> PR b -> PR c -> PR (a,b,c)
{-# INLINE dPR_3 #-}
dPR_3 pra prb prc
  = PR {
      lengthPR     = lengthPR_3
    , emptyPR      = emptyPR_3 pra prb prc
    , replicatePR  = replicatePR_3 pra prb prc
    , replicatelPR = replicatelPR_3 pra prb prc
    , repeatPR     = repeatPR_3 pra prb prc
    , indexPR      = indexPR_3 pra prb prc
    , bpermutePR   = bpermutePR_3 pra prb prc
    , appPR        = appPR_3 pra prb prc
    , applPR       = applPR_3 pra prb prc
    , packPR       = packPR_3 pra prb prc
    , combine2PR   = combine2PR_3 pra prb prc
    }

{-# INLINE lengthPR_3 #-}
lengthPR_3 (P_3 n# _ _ _) = n#

{-# INLINE emptyPR_3 #-}
emptyPR_3 pra prb prc = P_3 0# (emptyPR pra) (emptyPR prb) (emptyPR prc)

{-# INLINE replicatePR_3 #-}
replicatePR_3 pra prb prc n# ~(a,b,c)
  = P_3 n# (replicatePR pra n# a)
           (replicatePR prb n# b)
           (replicatePR prc n# c)

{-# INLINE replicatelPR_3 #-}
replicatelPR_3 pra prb prc n# ns (P_3 _ as bs cs)
  = P_3 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs)
           (replicatelPR prc n# ns cs)

{-# INLINE repeatPR_3 #-}
repeatPR_3 pra prb prc n# (P_3 m# as bs cs)
  = P_3 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)
                   (repeatPR prc n# cs)

{-# INLINE indexPR_3 #-}
indexPR_3 pra prb prc (P_3 n# as bs cs) i#
  = (indexPR pra as i#, indexPR prb bs i#, indexPR prc cs i#)

{-# INLINE bpermutePR_3 #-}
bpermutePR_3 pra prb prc (P_3 _ as bs cs) is
  = P_3 (lengthPA_Int# is) (bpermutePR pra as is)
                           (bpermutePR prb bs is)
                           (bpermutePR prc cs is)
{-# INLINE appPR_3 #-}
appPR_3 pra prb prc (P_3 m# as1 bs1 cs1) (P_3 n# as2 bs2 cs2)
  = P_3 (m# +# n#) (appPR pra as1 as2) (appPR prb bs1 bs2) (appPR prc cs1 cs2)

{-# INLINE applPR_3 #-}
applPR_3 pra prb prc is (P_3 m# as1 bs1 cs1) js (P_3 n# as2 bs2 cs2)
  = P_3 (m# +# n#) (applPR pra is as1 js as2)
                   (applPR prb is bs1 js bs2)
                   (applPR prc is cs1 js cs2)

{-# INLINE packPR_3 #-}
packPR_3 pra prb prc (P_3 _ as bs cs) n# sel#
  = P_3 n# (packPR pra as n# sel#)
           (packPR prb bs n# sel#)
           (packPR prc cs n# sel#)

{-# INLINE combine2PR_3 #-}
combine2PR_3 pra prb prc n# sel# is# (P_3 _ as1 bs1 cs1)
                                     (P_3 _ as2 bs2 cs2)
  = P_3 n# (combine2PR pra n# sel# is# as1 as2)
           (combine2PR prb n# sel# is# bs1 bs2)
           (combine2PR prc n# sel# is# cs1 cs2)


zip3PA# :: PA a -> PA b -> PA c
        -> PArray a -> PArray b -> PArray c -> PArray (a,b,c)
{-# INLINE_PA zip3PA# #-}
zip3PA# pa pb pc xs ys zs = P_3 (lengthPA# pa xs) xs ys zs

dPR_4 :: PR a -> PR b -> PR c -> PR d -> PR (a,b,c,d)
{-# INLINE dPR_4 #-}
dPR_4 pra prb prc prd
  = PR {
      lengthPR     = lengthPR_4
    , emptyPR      = emptyPR_4 pra prb prc prd
    , replicatePR  = replicatePR_4 pra prb prc prd
    , replicatelPR = replicatelPR_4 pra prb prc prd
    , repeatPR     = repeatPR_4 pra prb prc prd
    , indexPR      = indexPR_4 pra prb prc prd
    , bpermutePR   = bpermutePR_4 pra prb prc prd
    , appPR        = appPR_4 pra prb prc prd
    , applPR       = applPR_4 pra prb prc prd
    , packPR       = packPR_4 pra prb prc prd
    , combine2PR   = combine2PR_4 pra prb prc prd
    }

{-# INLINE lengthPR_4 #-}
lengthPR_4 (P_4 n# _ _ _ _) = n#

{-# INLINE emptyPR_4 #-}
emptyPR_4 pra prb prc prd = P_4 0# (emptyPR pra)
                                   (emptyPR prb)
                                   (emptyPR prc)
                                   (emptyPR prd)

{-# INLINE replicatePR_4 #-}
replicatePR_4 pra prb prc prd n# ~(a,b,c,d)
  = P_4 n# (replicatePR pra n# a)
           (replicatePR prb n# b)
           (replicatePR prc n# c)
           (replicatePR prd n# d)

{-# INLINE replicatelPR_4 #-}
replicatelPR_4 pra prb prc prd n# ns (P_4 _ as bs cs ds)
  = P_4 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs)
           (replicatelPR prc n# ns cs)
           (replicatelPR prd n# ns ds)

{-# INLINE repeatPR_4 #-}
repeatPR_4 pra prb prc prd n# (P_4 m# as bs cs ds)
  = P_4 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)
                   (repeatPR prc n# cs)
                   (repeatPR prd n# ds)

{-# INLINE indexPR_4 #-}
indexPR_4 pra prb prc prd (P_4 n# as bs cs ds) i#
  = (indexPR pra as i#,
     indexPR prb bs i#,
     indexPR prc cs i#,
     indexPR prd ds i#)

{-# INLINE bpermutePR_4 #-}
bpermutePR_4 pra prb prc prd (P_4 _ as bs cs ds) is
  = P_4 (lengthPA_Int# is) (bpermutePR pra as is)
                           (bpermutePR prb bs is)
                           (bpermutePR prc cs is)
                           (bpermutePR prd ds is)

{-# INLINE appPR_4 #-}
appPR_4 pra prb prc prd (P_4 m# as1 bs1 cs1 ds1) (P_4 n# as2 bs2 cs2 ds2)
  = P_4 (m# +# n#) (appPR pra as1 as2)
                   (appPR prb bs1 bs2)
                   (appPR prc cs1 cs2)
                   (appPR prd ds1 ds2)

{-# INLINE applPR_4 #-}
applPR_4 pra prb prc prd is (P_4 m# as1 bs1 cs1 ds1) js (P_4 n# as2 bs2 cs2 ds2)
  = P_4 (m# +# n#) (applPR pra is as1 js as2)
                   (applPR prb is bs1 js bs2)
                   (applPR prc is cs1 js cs2)
                   (applPR prd is ds1 js ds2)

{-# INLINE packPR_4 #-}
packPR_4 pra prb prc prd (P_4 _ as bs cs ds) n# sel#
  = P_4 n# (packPR pra as n# sel#)
           (packPR prb bs n# sel#)
           (packPR prc cs n# sel#)
           (packPR prd ds n# sel#)

{-# INLINE combine2PR_4 #-}
combine2PR_4 pra prb prc prd n# sel# is# (P_4 _ as1 bs1 cs1 ds1)
                                         (P_4 _ as2 bs2 cs2 ds2)
  = P_4 n# (combine2PR pra n# sel# is# as1 as2)
           (combine2PR prb n# sel# is# bs1 bs2)
           (combine2PR prc n# sel# is# cs1 cs2)
           (combine2PR prd n# sel# is# ds1 ds2)

dPR_5 :: PR a -> PR b -> PR c -> PR d -> PR e -> PR (a,b,c,d,e)
{-# INLINE dPR_5 #-}
dPR_5 pra prb prc prd pre
  = PR {
      lengthPR     = lengthPR_5
    , emptyPR      = emptyPR_5 pra prb prc prd pre
    , replicatePR  = replicatePR_5 pra prb prc prd pre
    , replicatelPR = replicatelPR_5 pra prb prc prd pre
    , repeatPR     = repeatPR_5 pra prb prc prd pre
    , indexPR      = indexPR_5 pra prb prc prd pre
    , bpermutePR   = bpermutePR_5 pra prb prc prd pre
    , appPR        = appPR_5 pra prb prc prd pre
    , applPR       = applPR_5 pra prb prc prd pre
    , packPR       = packPR_5 pra prb prc prd pre
    , combine2PR   = combine2PR_5 pra prb prc prd pre
    }

{-# INLINE lengthPR_5 #-}
lengthPR_5 (P_5 n# _ _ _ _ _) = n#

{-# INLINE emptyPR_5 #-}
emptyPR_5 pra prb prc prd pre
  = P_5 0# (emptyPR pra)
           (emptyPR prb)
           (emptyPR prc)
           (emptyPR prd)
           (emptyPR pre)

{-# INLINE replicatePR_5 #-}
replicatePR_5 pra prb prc prd pre n# ~(a,b,c,d,e)
  = P_5 n# (replicatePR pra n# a)
           (replicatePR prb n# b)
           (replicatePR prc n# c)
           (replicatePR prd n# d)
           (replicatePR pre n# e)

{-# INLINE replicatelPR_5 #-}
replicatelPR_5 pra prb prc prd pre n# ns (P_5 _ as bs cs ds es)
  = P_5 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs)
           (replicatelPR prc n# ns cs)
           (replicatelPR prd n# ns ds)
           (replicatelPR pre n# ns es)

{-# INLINE repeatPR_5 #-}
repeatPR_5 pra prb prc prd pre n# (P_5 m# as bs cs ds es)
  = P_5 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)
                   (repeatPR prc n# cs)
                   (repeatPR prd n# ds)
                   (repeatPR pre n# es)

{-# INLINE indexPR_5 #-}
indexPR_5 pra prb prc prd pre (P_5 n# as bs cs ds es) i#
  = (indexPR pra as i#,
     indexPR prb bs i#,
     indexPR prc cs i#,
     indexPR prd ds i#,
     indexPR pre es i#)

{-# INLINE bpermutePR_5 #-}
bpermutePR_5 pra prb prc prd pre (P_5 _ as bs cs ds es) is
  = P_5 (lengthPA_Int# is) (bpermutePR pra as is)
                           (bpermutePR prb bs is)
                           (bpermutePR prc cs is)
                           (bpermutePR prd ds is)
                           (bpermutePR pre es is)

{-# INLINE appPR_5 #-}
appPR_5 pra prb prc prd pre (P_5 m# as1 bs1 cs1 ds1 es1)
                            (P_5 n# as2 bs2 cs2 ds2 es2)
  = P_5 (m# +# n#) (appPR pra as1 as2)
                   (appPR prb bs1 bs2)
                   (appPR prc cs1 cs2)
                   (appPR prd ds1 ds2)
                   (appPR pre es1 es2)

{-# INLINE applPR_5 #-}
applPR_5 pra prb prc prd pre is (P_5 m# as1 bs1 cs1 ds1 es1)
                             js (P_5 n# as2 bs2 cs2 ds2 es2)
  = P_5 (m# +# n#) (applPR pra is as1 js as2)
                   (applPR prb is bs1 js bs2)
                   (applPR prc is cs1 js cs2)
                   (applPR prd is ds1 js ds2)
                   (applPR pre is es1 js es2)

{-# INLINE packPR_5 #-}
packPR_5 pra prb prc prd pre (P_5 _ as bs cs ds es) n# sel#
  = P_5 n# (packPR pra as n# sel#)
           (packPR prb bs n# sel#)
           (packPR prc cs n# sel#)
           (packPR prd ds n# sel#)
           (packPR pre es n# sel#)

{-# INLINE combine2PR_5 #-}
combine2PR_5 pra prb prc prd pre n# sel# is# (P_5 _ as1 bs1 cs1 ds1 es1)
                                             (P_5 _ as2 bs2 cs2 ds2 es2)
  = P_5 n# (combine2PR pra n# sel# is# as1 as2)
           (combine2PR prb n# sel# is# bs1 bs2)
           (combine2PR prc n# sel# is# cs1 cs2)
           (combine2PR prd n# sel# is# ds1 ds2)
           (combine2PR pre n# sel# is# es1 es2)

data Sum2 a b = Alt2_1 a | Alt2_2 b
data Sum3 a b c = Alt3_1 a | Alt3_2 b | Alt3_3 c

data instance PArray (Sum2 a b)
  = PSum2 Int# PArray_Int# PArray_Int# (PArray a)
                                      (PArray b)

data instance PArray (Sum3 a b c)
  = PSum3 Int# PArray_Int# PArray_Int# (PArray a)
                                       (PArray b)
                                       (PArray c)

dPR_Sum2 :: PR a -> PR b -> PR (Sum2 a b)
{-# INLINE dPR_Sum2 #-}
dPR_Sum2 pra prb = PR {
                     lengthPR     = lengthPR_Sum2
                   , emptyPR      = emptyPR_Sum2 pra prb
                   , replicatePR  = replicatePR_Sum2 pra prb
                   , replicatelPR = replicatelPR_Sum2 pra prb
                   , repeatPR     = repeatPR_Sum2 pra prb
                   , indexPR      = indexPR_Sum2 pra prb
                   , bpermutePR   = bpermutePR_Sum2 pra prb 
                   , appPR        = appPR_Sum2 pra prb 
                   , applPR       = applPR_Sum2 pra prb 
                   , packPR       = packPR_Sum2 pra prb 
                   , combine2PR   = combine2PR_Sum2 pra prb 
                   }
 
{-# INLINE lengthPR_Sum2 #-}
lengthPR_Sum2 (PSum2 n# _ _ _ _) = n#

{-# INLINE emptyPR_Sum2 #-}
emptyPR_Sum2 pra prb
  = PSum2 0# emptyPA_Int# emptyPA_Int# (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_Sum2 #-}
replicatePR_Sum2 pra prb n# p
  = PSum2 n# (replicatePA_Int# n# (case p of Alt2_1 _ -> 0#
                                             Alt2_2 _ -> 1#))
             (upToPA_Int# n#)
             (case p of Alt2_1 x -> replicatePR pra n# x
                        _        -> emptyPR pra)
             (case p of Alt2_2 y -> replicatePR prb n# y
                        _        -> emptyPR prb)

{-# INLINE replicatelPR_Sum2 #-}
replicatelPR_Sum2 pra prb n# mults (PSum2 m# sel# is# as bs) =
  case sumPA_Int# (unsafe_zipWithPA_Int# (*) sel# mults) of 
    an1# -> PSum2 n# sel' is' as' bs'  
            where
              as'       = replicatelPR pra an1# alt1mults as
              bs'       = replicatelPR prb (n# -# an1#) alt2mults bs
              sel'      = replicatelPA_Int# n# sel# mults
              alt1mults = packPA_Int# mults n# (mapU (==0) sel#)
              alt2mults = packPA_Int# mults n# (mapU (==1) sel#)
              is'       = combine2'PA_Int# sel' (enumFromToU 0 (I# (an1# -#  1#))) (enumFromToU 0 (I#(n# -# an1# -#  1#)))


repeatPR_Sum2 pra prb n# (PSum2 m# sel# is# as bs) = 
  case (sumPA_Int# sel#) of 
    an1# -> PSum2 (m# *# n#) sel' is' as' bs' 
            where
              as'  = repeatPR pra n# as
              bs'  = repeatPR prb n# bs
              sel' = repeatPA_Int# n# sel#
              is'  = combine2'PA_Int# sel' (enumFromToU 0 (I#(an1# -#  1#))) (enumFromToU 0 (I#(n# *# m# -# an1# -#  1#)))



{-# INLINE indexPR_Sum2 #-}
indexPR_Sum2 pra prb (PSum2 n# sel# is# as bs) i#
  = case indexPA_Int# sel# i# of
      0# -> Alt2_1 (indexPR pra as (indexPA_Int# is# i#))
      _  -> Alt2_2 (indexPR prb bs (indexPA_Int# is# i#))


bpermutePR_Sum2 pra prb  is = error "bpermutePR_Sum2 nyi"

appPR_Sum2 pra prb (PSum2 n1# sel1# _ as1 bs1) (PSum2 n2# sel2# _ as2 bs2) = 
  PSum2 (n1# +# n2#) (appPA_Int# sel1# sel2#) (error "ind in appPR_Sum2 nyi") (appPR pra as1 as2) (appPR prb bs1 bs2)


applPR_Sum2 pra prb _ _  = error "applPR_Sum2 nyi"

packPR_Sum2 :: PR a -> PR b -> PArray (Sum2 a b) -> Int# -> PArray_Bool# -> PArray (Sum2 a b)
packPR_Sum2 pra prb  (PSum2 n# sel# _ as bs) m# flags = let
  sel' = packU sel# flags 
  in
  case sumPA_Int# sel' of
    k# -> PSum2 m# sel' is as' bs' 
            where 
              aFlags = packU (mapU (==0) sel#) flags
              bFlags = packU (mapU (==1) sel#) flags
              as'  = packPR pra as (m# -# k#) aFlags
              bs'  = packPR prb bs k# bFlags
              is   = error "packPR_Sum2 index not impl"


combine2PR_Sum2:: PR a -> PR b -> Int# -> PArray_Int# -> PArray_Int#
                              -> PArray (Sum2 a b) -> PArray (Sum2 a b) -> PArray (Sum2 a b)
combine2PR_Sum2 pra prb n# sel# is# (PSum2 m1# sel1# _ as1 bs1) (PSum2 m2# sel2# _ as2 bs2) = 
     case   (sel'Bool, nsel'Bool) of
       (s1#, s2#) ->  trace ("n  = " ++ show (I# n#)  ++ "\n" ++
                             "m1 = " ++ show (I# m1#)  ++ "\n" ++
                             "m2 = " ++ show (I# m2#)  ++ "\n" ++
                             "sel = " ++ show sel#  ++ "\n" ++
                             "sel1 = " ++ show sel1#  ++ "\n" ++
                             "sel2 = " ++ show sel2#  ++ "\n" ++
                             "selB = " ++ show sel'Bool  ++ "\n" ++
                             "nselB = " ++ show nsel'Bool  ++ "\n" ++
                             "sel' = " ++ show sel'  ++ "\n"
                             )
                          $ 
                         PSum2  n# sel' (error "combine2PR_Sum2 index nyi") as' bs'
                      where     
                        as# = lengthPR pra as1 +#   lengthPR pra as2
                        bs# = lengthPR prb bs1 +#   lengthPR prb bs2
                        asel = packPA_Int# sel# as# s1#
                        bsel = packPA_Int# sel# bs# s2#
                        as' = trace ("cb1: " ++ show asel) $ combine2PR pra as# asel  is# as1 as2 
                        bs' = trace ("cb2: " ++ show bsel) $ combine2PR prb bs# bsel is# bs1 bs2
     where
       sel' = combine2PA_Int# n# sel# is#  sel1# sel2#
       sel'Bool  = mapU (==0) sel'
       nsel'Bool = mapU (==1) sel'


dPR_Sum3 :: PR a -> PR b -> PR c -> PR (Sum3 a b c)
{-# INLINE dPR_Sum3 #-}
dPR_Sum3 pra prb prc
  = PR {
     lengthPR    = lengthPR_Sum3
   , emptyPR     = emptyPR_Sum3 pra prb prc
   , replicatePR = replicatePR_Sum3 pra prb prc
   , indexPR     = indexPR_Sum3 pra prb prc
   }

{-# INLINE lengthPR_Sum3 #-}
lengthPR_Sum3 (PSum3 n# _ _ _ _ _) = n#

{-# INLINE emptyPR_Sum3 #-}
emptyPR_Sum3 pra prb prc
  = PSum3 0# emptyPA_Int# emptyPA_Int# (emptyPR pra)
                                       (emptyPR prb)
                                       (emptyPR prc)

{-# INLINE replicatePR_Sum3 #-}
replicatePR_Sum3 pra prb prc n# p
  = PSum3 n# (replicatePA_Int# n# (case p of Alt3_1 _ -> 0#
                                             Alt3_2 _ -> 1#
                                             Alt3_3 _ -> 2#))
             (upToPA_Int# n#)
             (case p of Alt3_1 x -> replicatePR pra n# x
                        _        -> emptyPR pra)
             (case p of Alt3_2 x -> replicatePR prb n# x
                        _        -> emptyPR prb)
             (case p of Alt3_3 x -> replicatePR prc n# x
                        _        -> emptyPR prc)

{-# INLINE indexPR_Sum3 #-}
indexPR_Sum3 pra prb prc (PSum3 n# sel# is# as bs cs) i#
  = case indexPA_Int# sel# i# of
      0# -> Alt3_1 (indexPR pra as (indexPA_Int# is# i#))
      1# -> Alt3_2 (indexPR prb bs (indexPA_Int# is# i#))
      _  -> Alt3_3 (indexPR prc cs (indexPA_Int# is# i#))

data instance PArray (PArray a)
  = PNested Int# PArray_Int# PArray_Int# (PArray a)

dPR_PArray :: PR a -> PR (PArray a)
{-# INLINE dPR_PArray #-}
dPR_PArray pr = PR {
                  lengthPR     = lengthPR_PArray
                , emptyPR      = emptyPR_PArray pr
                , replicatePR  = replicatePR_PArray pr
                , replicatelPR = replicatelPR_PArray pr
                , repeatPR     = repeatPR_PArray pr
                , bpermutePR   = bpermutePR_PArray pr
                , appPR        = appPR_PArray pr
                , applPR       = applPR_PArray pr
                , packPR       = packPR_PArray pr
                , combine2PR   = combine2PR_PArray pr
                }

{-# INLINE lengthPR_PArray #-}
lengthPR_PArray (PNested n# _ _ _) = n#

{-# INLINE nested_lengthPA #-}
nested_lengthPA xss = I# (lengthPR_PArray xss)

{-# INLINE emptyPR_PArray #-}
emptyPR_PArray pr = PNested 0# emptyPA_Int# emptyPA_Int# (emptyPR pr)

{-# INLINE replicatePR_PArray #-}
replicatePR_PArray pr n# xs
  = PNested n# lens
               (unsafe_scanPA_Int# (+) 0 lens)
               (repeatPR pr n# xs)
  where
    lens = replicatePA_Int# n# (lengthPR pr xs)

{- INLINE bpermutePR_PArray 3-}
bpermutePR_PArray pr (PNested n# xslens xsInds xs) is = PNested n# xslens' xsInds' xs'
  where
    xslens' = bpermutePA_Int# xslens is
    xsInds' = unsafe_scanPA_Int# (+) 0 xslens'
    is1     = bpermutePA_Int# xsInds is  
    is2     = zipWithU (\x -> \y -> x + y - 1) xslens' is1
    ps      = concatSU $ enumFromToSU is1 is2    
    xs'     = bpermutePR pr xs ps

{-# INLINE appPR_PArray #-}
appPR_PArray pr (PNested n# xslens xsInds xs) (PNested m# yslens ysInds ys) = 
   PNested (n# +# m#) (appPA_Int# xslens yslens) (appPA_Int# xsInds ysInds)  (appPR pr xs ys)

{-# INLINE applPR_PArray #-}
-- applPR_PArray:: PR a -> USegd -> PArray a -> USegd -> PArray a -> PArray a
applPR_PArray pr is1  xn@(PNested n# xslens xsInds xs) is2 yn@(PNested m# yslens ysInds ys) = 
   PNested (n# +# m#) lens ids xys
   where 
     lens   = appPA_Int#  xslens yslens 
     xsSegd = sumSU (is1 >: xslens)
     ysSegd = sumSU (is2 >: yslens)
     ids    = unsafe_scanPA_Int# (+) 0 lens
     xlen# = lengthPR pr xs
     ylen# = lengthPR pr ys
     len#  = xlen# +# ylen#
     (PNested _ _ _ xys)   = combine2PR_PArray pr len# isel (error "tmp ind nyi") 
       (PNested n# xsSegd (error "bla1") xs)  (PNested n# ysSegd (error "bla1") ys)
     isel   = mapU (\x -> if odd x then (0::Int) else 1) $ enumFromToU 1 (2* lengthU xslens)


-- FIXME: compute indices more efficiently?
{-# INLINE repeatPR_PArray #-}
repeatPR_PArray pr n# (PNested m# lens _ xs)
  = PNested (m# *# n#) lens'
                       (unsafe_scanPA_Int# (+) 0 lens')
                       (repeatPR pr n# xs)
  where
    lens' = repeatPA_Int# n# lens

{-# INLINE replicatelPR_PArray #-}
replicatelPR_PArray pr n# ns (PNested _ lens idxs xs)
  = PNested n# new_lens new_idxs (bpermutePR pr xs indices)
  where
    new_lens = replicateEachU (I# n#) ns lens
    new_idxs = scanU (+) 0 new_lens
    starts = replicateEachU (I# n#) ns idxs
    ends   = replicateEachU (I# n#) ns
           $ zipWithU (\i l -> i+l-1) idxs lens

    indices = enumFromToEachU (sumU (zipWithU (*) ns lens))
            $ zipU starts ends

{-# INLINE packPR_PArray #-}
packPR_PArray pr (PNested _ lens _ xs) n# bs
  = PNested n# lens' idxs'
     (packPR pr xs (sumPA_Int# lens')
                   (replicatelPA_Bool# (lengthPR pr xs) lens bs))
  where
    lens' = pack'PA_Int# lens bs
    idxs' = unsafe_scanPA_Int# (+) 0 lens'

{-# INLINE combine2PR_PArray #-}
combine2PR_PArray pr n# sel is (PNested _ lens1 idxs1 xs)
                               (PNested _ lens2 idxs2 ys)
  = PNested n# lens idxs (combine2PR pr len# sel' is' xs ys)
  where
    lens = combine2PA_Int# n# sel is lens1 lens2
    idxs = unsafe_scanPA_Int# (+) 0 lens

    xlen# = lengthPR pr xs
    ylen# = lengthPR pr ys
    len#  = xlen# +# ylen#

    sel' = replicatelPA_Int# len# lens sel
    is'  = mapU pick
         . zipU sel'
         . scanU index (0 :*: 0)
         $ mapU init sel'
         
    init 0 = 1 :*: 0
    init _ = 0 :*: 1

    index (i1 :*: j1) (i2 :*: j2) = (i1+i2 :*: j1+j2)

    pick (0 :*: (i :*: j)) = i
    pick (_ :*: (i :*: j)) = j

concatPA# :: PArray (PArray a) -> PArray a
{-# INLINE_PA concatPA# #-}
concatPA# (PNested _ _ _ xs) = xs

fromSUArrPA :: PrimPA a => Int -> Int -> SUArr a -> PArray (PArray a)
{-# INLINE fromSUArrPA #-}
fromSUArrPA (I# m#) n xss = PNested m# (lengthsSU xss)
                                       (indicesSU xss)
                                       (fromUArrPA n (concatSU xss))

toSUArrPA :: PrimPA a => PArray (PArray a) -> SUArr a
{-# INLINE toSUArrPA #-}
toSUArrPA (PNested _ lens idxs xs) = toUSegd (zipU lens idxs) >: toUArrPA xs

fromSUArrPA_2 :: (PrimPA a, PrimPA b)
              => Int -> Int -> SUArr (a :*: b) -> PArray (PArray (a, b))
{-# INLINE fromSUArrPA_2 #-}
fromSUArrPA_2 (I# m#) n pss = PNested m# (lengthsSU pss)
                                         (indicesSU pss)
                                         (fromUArrPA_2 n (concatSU pss))

fromSUArrPA' :: PrimPA a => SUArr a -> PArray (PArray a)
{-# INLINE fromSUArrPA' #-}
fromSUArrPA' xss = fromSUArrPA (lengthSU xss)
                               (lengthU (concatSU xss))
                               xss

fromSUArrPA_2' :: (PrimPA a, PrimPA b)
                => SUArr (a :*: b) -> PArray (PArray (a, b))
{-# INLINE fromSUArrPA_2' #-}
fromSUArrPA_2' pss = fromSUArrPA_2 (lengthSU pss)
                                   (lengthU (concatSU pss))
                                   pss

