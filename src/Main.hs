module Main(main) where

import Types
import LSystem
import Codec.BMP
import Data.Word
import Data.ByteString
import System.IO
import Data.Vect.Float.Base
import Data.Maybe
import System.Random

sWidth = 600 :: Float
sHeight = 400 :: Float

iPI = (1/3.141593)
skyCol = (Vec3 100 100 180)
maxDepth = 4 :: Int

--SceneData
--objects = [Sphere (Vec3 (-6) (-4) 25) 6 (Material (Vec3 0.2 0.7 0.2) 0.01),Sphere (Vec3 (5) (-2) 18) 4 (Material (Vec3 0.2 0.2 0.8) 0.5),Sphere (Vec3 (-2) 0 10) 2 (Material (Vec3 0.8 0.2 0.2) 0.2), Plane (Vec3 0 2 0) (Vec3 0 (1) 0) (Material (Vec3 0.2 0.2 0.2) 0.1), Cylinder (Vec3 0 (-3) 20) (Vec3 5 (-2) 25) 0.5 (Material (Vec3 0.5 0.5 0.5) 0)] :: Objects
lights = [PointLight (Vec3 (0) (-12) 15) (Vec3 1 1 1) (Vec3 250000 250000 250000), AmbientLight (Vec3 40 40 70)] :: Lights
--planes = [Cylinder (Vec3 0 2 20) (Vec3 0 (-3) 20) 0.5 (Material (Vec3 0.5 0.5 0.5) 0),Cylinder (Vec3 0 (-3) 20) (Vec3 2 (-5) 20) 0.2 (Material (Vec3 0.9 0.5 0.5) 0), Plane (Vec3 0 2 0) (Vec3 0 (1) 0) (Material (Vec3 0.2 0.2 0.2) 0.1)]
system = [Plane (Vec3 0 2 0) (Vec3 0 (1) 0) (Material (Vec3 0.2 0.2 0.2) 0.1 False 1), Sphere (Vec3 (-2) (0) 8) 1.8 (Material (Vec3 0.4 0.4 0.4) 0.2 True 1.15), Sphere (Vec3 (2) (0) 8) 1.8 (Material (Vec3 0.4 0.4 0.4) 0.2 True 1.3)] ++ (generateLSystem (Vec3 (-1) 2 12) (Vec3 0 (-1) 0) 3 0.9  0.1 4)
scene = (system, lights)

--Helper Function
multVec3 :: Vec3 -> Vec3 -> Vec3
multVec3 (Vec3 a b c) (Vec3 d e f) = Vec3 (a*d) (b*e) (c*f)

minInter :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
minInter Nothing Nothing = Nothing
minInter Nothing x = x
minInter x Nothing = x
minInter (Just (Intersection a c e s)) (Just (Intersection b d f p)) = if a < b then Just (Intersection a c e s) else Just (Intersection b d f p)

sumVec3 :: [Vec3] -> Vec3
sumVec3 [] = (Vec3 0 0 0)
sumVec3 (x:xs) = x &+ (sumVec3 xs)

rayReflect :: Vec3 -> Vec3 -> Vec3
rayReflect ray n = ray &- (n &* (2*(dotprod n ray)))

clamp :: Float -> Float
clamp x | x > 255 = 255
        | otherwise = x

clampW :: Word8 -> Word8
clampW x | x > 255 = 255
        | otherwise = x
        
fromVec3 :: Vec3 -> Col
fromVec3 (Vec3 r g b) = [fromInteger (round r), fromInteger (round g), fromInteger (round b), 255]

--Intersection Functions
closestIntersect :: [Maybe Intersection] -> Maybe Intersection
closestIntersect [] = Nothing
closestIntersect [x] = if isNothing x then Nothing else x
closestIntersect (x:xs) = minInter x (closestIntersect xs)

getIntersection :: Ray -> Objects -> Maybe Intersection
getIntersection (orig, dir) xs = closestIntersect (Prelude.map (intersect (orig, dir)) xs)

intersect :: Ray -> Shape -> Maybe Intersection
intersect (orig, dir) (Plane p norm mat)
                                | denom < 1e-6 = Nothing
                                | t < 0 = Nothing
                                | otherwise = Just (Intersection t (norm &* (-1)) mat (Plane p norm mat))
                                where
                                    denom = dotprod dir norm
                                    p1 = p &- orig
                                    t | denom == 0 = 1
                                      | otherwise = (dotprod p1 norm) / denom
intersect (orig, dir) (Cylinder p e r mat)
                                | det < 0 = Nothing
                                | t0 > 0 && hit0 && in0 = Just (Intersection t0 (normalize (ht0 &- ((p &+ (va &* ((dotprod (ht0 &- p) va)/(dotprod va va))))))) mat (Cylinder p e r mat))
                                | t1 > 0 && hit1 && in1 = Just (Intersection t0 ((normalize (ht1 &- ((p &+ (va &* ((dotprod (ht1 &- p) va)/(dotprod va va))))))) &* (-1)) mat (Cylinder p e r mat))
                                | otherwise = Nothing
                                where
                                    l = len (p &- e)
                                    v = dir
                                    va = normalize (e &- p)
                                    dP = orig &- p
                                    tA = len (v &- (va &* (dotprod v va)))
                                    b = 2*(dotprod (v &- (va &* (dotprod v va))) (dP &- (va &* (dotprod dP va))))
                                    tC = len (dP &- (va &* (dotprod dP va)))
                                    a = tA * tA
                                    c = (tC * tC) - (r * r)
                                    det = b*b - (4*a*c)
                                    t0 | det < 0 = (-5)
                                       | otherwise = ((-b) - (sqrt (det))) / (2*a) 
                                    t1 | det < 0 = (-5)
                                       | otherwise = ((-b) + (sqrt (det))) / (2*a)
                                    ht0 = orig &+ (dir &* t0)
                                    ht1 = orig &+ (dir &* t1)
                                    hit0 = (len (va &* (dotprod (ht0 &- p) va))) < l
                                    hit1 = (len (va &* (dotprod (ht1 &- p) va))) < l
                                    in0 = (dotprod (ht0 &- p) va) > 0
                                    in1 = (dotprod (ht1 &- p) va) > 0
 

intersect (orig, dir) (Sphere p radius mat)
                                | det < 0 = Nothing
                                | t0 > eps = Just (Intersection t0 (normalize ((orig &+ (dir &* t0)) &- p)) mat (Sphere p radius mat))
                                | t1 > eps = Just (Intersection t1 (normalize ((orig &+ (dir &* t1)) &- p)) mat (Sphere p radius mat))
                                | otherwise = Nothing
                                where
                                    op = p &- orig
                                    eps = 1e-3
                                    b = dotprod op dir
                                    det = b*b - (dotprod op op) + radius*radius
                                    sdet = sqrt det
                                    t0 = b-sdet
                                    t1 = b+sdet
            
generateRay :: Int -> Int -> Ray
generateRay x y = ((Vec3 0 0 0), (normalize (Vec3 cx cy 4)))
                 where
                    tLx = -3
                    tLy = 2
                    cx = tLx + (((fromIntegral x)/1200)*6)
                    cy = tLy - (((fromIntegral y)/800)*4)

computeLighting :: Material -> Scene -> Shape -> Vec3 -> Vec3 -> Light -> Vec3
computeLighting (Material col ref tran ri) (objs, ls) obj pos norm (PointLight p c i) | inShadow = Vec3 0 0 0 
                                                           | otherwise = multVec3 ((col &* iPI) &* (1/(lMag*lMag)) &* (max 0 dot)) i
                                                            where
                                                                lVector = p &- pos
                                                                lMag = len lVector                                                         
                                                                ldir = normalize (lVector)
                                                                inShadow = not (isNothing (getIntersection (pos, ldir) [x | x<-objs, not (shapeEq x obj)]))
                                                                dot = (dotprod norm ldir) 
computeLighting (Material c ref tran ri) _ _ _ _ (AmbientLight l) = multVec3 c l

                                                       
getColor :: Ray -> Scene -> Int -> Vec3
getColor (orig, dir) (objs, ls) d | isNothing x = skyCol
                                  | otherwise = (Vec3 (clamp r) (clamp g) (clamp b))
                                    where
                                        x = getIntersection (orig, dir) objs
                                        (Intersection t n mat obj) = fromMaybe (Intersection 0 (Vec3 0 0 0) (Material (Vec3 0 0 0) 0 True 0) (Sphere (Vec3 0 0 0) 2 (Material (Vec3 0 0 0) 0 False 0))) x 
                                        (Material alb ref tran ri) = mat
                                        hit = orig &+ (dir &* t)
                                        c1 = dotprod dir n
                                        inside = c1 > 0
                                        c = abs c1
                                        phi | inside = ri
                                            | otherwise = 1/ri
                                        s2 | tran = phi*phi * (1 - (c*c))
                                           | otherwise = 1                                                                    
                                        (Vec3 r g b) | tran && d <= maxDepth = getColor (hit, normalize ((dir &* phi) &+ (n &* (phi*c - (sqrt (1 - s2)))))) (objs, ls) (d+1)
                                                     | ref > 0 && d <= maxDepth = sumVec3 [computeLighting mat (objs, ls) obj hit n l | l<-ls] &+ ((getColor (hit, (rayReflect dir n)) ([x | x<-objs,not (shapeEq x obj)], ls) (d+1)) &* ref)                                                
                                                     | d <= maxDepth = sumVec3 [computeLighting mat (objs, ls) obj hit n l | l<-ls]
                                                     | otherwise = skyCol
getColorAA :: Ray -> Scene -> Int -> Float -> Vec3
getColorAA (orig, dir) (objs, ls) d aa =undefined

generateImage :: BMP
generateImage = packRGBA32ToBMP32 1200 800 (pack (Prelude.concat [fromVec3 (getColor (generateRay x y) scene 0)| y<-[1..800], x<-[1..1200]]))

generateImageAA :: Float -> BMP
generateImageAA aa = packRGBA32ToBMP32 600 400 (pack (Prelude.concat [fromVec3 (getColorAA (generateRay x y) scene 0 aa)| y<-[1..400], x<-[1..600]])) 
    
main :: IO ()
main = writeBMP "image.bmp" generateImage








