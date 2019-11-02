module Types where

import Data.Vect.Float.Base
import Data.Word

data Material = Material {albedo :: Vec3, reflection :: Float, transp :: Bool, ri :: Float}
data Intersection = Intersection {t :: Float, norm :: Vec3, mat :: Material, s :: Shape} 

data Shape = Sphere {center :: Vec3 , radius :: Float, m :: Material}  
           | AABB {bMin :: Vec3, bMax :: Vec3, m :: Material} 
           | Plane {center :: Vec3, normal :: Vec3, m :: Material} 
           | Cylinder {center :: Vec3, end :: Vec3, radius :: Float, m :: Material} 

data Light = PointLight {pos :: Vec3, color :: Vec3, intensity :: Vec3}
           | AmbientLight Vec3
           
--Types
type Col = [Word8]
type Ray = (Vec3, Vec3)
type Scene = (Objects, Lights)
type Objects = [Shape]
type Lights = [Light]

shapeEq :: Shape -> Shape -> Bool
shapeEq (Plane a b c) (Plane d e f) = (vecEq a d) && (vecEq b e) && (matEq c f)
shapeEq _ (Plane x y z) = False
shapeEq (Plane x y z) _ = False
shapeEq (Sphere x y z) (Sphere a b c) = (vecEq x a) && (y == b) && (matEq z c)
shapeEq (Sphere x y z) _ = False
shapeEq _ (Sphere x y z) = False
shapeEq (Cylinder x y z m) (Cylinder a b c d) = (vecEq x a) && (vecEq y b) && (z == c) && (matEq m d)
shapeEq (Cylinder x y z m) _ = False
shapeEq _ (Cylinder x y z m) = False

matEq :: Material -> Material -> Bool
matEq (Material w x y z) (Material a b c d) = (vecEq w a) && (x == b) && (y == c) && (z == d)

vecEq :: Vec3 -> Vec3 -> Bool
vecEq (Vec3 a b c) (Vec3 d e f) =  (a == d) && (b == e) && (c == f)
