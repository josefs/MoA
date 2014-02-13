{-# LANGUAGE TypeOperators #-}
module MoA where

import Prelude hiding ((!!),(<),zipWith,map,reverse,take,drop)

import Feldspar
import Feldspar.Vector
import Feldspar.Vector.Shape

op :: (ShapeConc sh1 sh2, Shapely sh1, Shapely sh2) =>
      (a -> b -> c) -> Pull sh1 a -> Pull sh2 b -> Pull (ShapeConcT sh1 sh2) c
op x v1 v2 = indexed (shapeConc (extent v1) (extent v2)) ixf
  where ixf sh = let (i,j) = splitIndex sh (extent v1)
                 in (v1 ! i) `x` (v2 ! j)

gradeup :: Pull DIM1 (Data Index) -> Pull DIM1 (Data Index)
gradeup vec = indexed (extent vec) (\ix -> vec !! (vec ! ix))

φ :: ShapeMap vec =>
     vec (sh :. Data Length) a -> vec (sh :. Data Length) a
φ vec = reverse vec

n `θ` vec = n < 0 ? rotateVecL (negate n) vec $ rotateVecR n vec

-- take and drop are not correct for negative arguments
n △ vec = take n vec

n ▽ vec = drop n vec

{-
shape :: Shaped vec => vec sh a -> Manifest DIM1 (Data Index)
shape vec = sugar (toList sh,[1])
  where sh = extent vec

transp :: Pull DIM1 (Data Index) -> Pull sh a -> Pull sh a
transp t vec = undefined
-}
instance (Num a, Shapely sh) => Num (Pull sh a) where
  v1 + v2 = zipWith (+) v1 v2
  v1 - v2 = zipWith (-) v1 v2
  v1 * v2 = zipWith (*) v1 v2
  abs v   = map abs v
  signum v = map signum v
  fromInteger i = indexed unitDim (const (fromInteger i))
