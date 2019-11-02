module LSystem (generateLSystem) where

import Data.Vect.Float.Base
import Types

generateLSystem :: Vec3 -> Vec3 -> Float -> Float -> Float -> Int -> [Shape]
generateLSystem orig z l lmult w d| d <= 0 = [Sphere orig 0.25 (Material (Vec3 0.3 0.8 0.3) 0 False 1)]
                                   | otherwise = [(Cylinder orig (end) w (Material (Vec3 0.6 0.6 0.6) 0 False 1))] ++ (Prelude.concat [generateLSystem end x (l*lmult) lmult (w*lmult) (d-1)| x <- xs]) 
                                   where 
                                    dir = normalize z
                                    end = orig &+ (dir &* l)
                                    xs = generateLeaves dir

generateLeaves :: Vec3 -> [Vec3]
generateLeaves (Vec3 x y z) = [normalize ((dir &* 1) &+ (n &+ c)), normalize ((dir &* 1) &+ (n &- c)), normalize ((dir &* 1) &+ (c &- n)), normalize ((dir &* 1) &+ (c &* (-1) &- n))]
                           where 
                              dir = (Vec3 x y z)
                              n = normalize ((Vec3 (0) (-z) y))
                              c = normalize (crossprod (Vec3 x y z) n)