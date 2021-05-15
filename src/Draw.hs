module Draw where

import MancalaTypes
import Graphics.Gloss.Interface.Pure.Game

xShift :: Int
xShift = 160

yShift :: Int
yShift = 100

x :: Int
x = 1600

y :: Int
y = 800

drawObj :: Int -> Float -> Float -> Float -> Picture -> Picture
drawObj 1 offset n1 n2 obj = (Translate (n1 + offset) n2 obj)
drawObj n offset n1 n2 obj = Pictures[(Translate (n1 + offset) n2 obj), (drawObj (n-1) offset (n1 + offset) n2 obj)]

drawDesk :: Info -> Picture
drawDesk (Info (x1: str1) (Out (y1: str2) m) ) = Pictures[picstr1, picstr2, picm1, picm2, pics1, pics2, (drawNums m)]
  where
    x0 = (fromIntegral (-(div x 2)))
    picstr1 = Translate x0 (fromIntegral (30 -(div y 2))) (Scale 0.5 0.5 (Color (greyN 0.7) (Text str1)))
    picstr2 = Translate x0 (fromIntegral ((div y 2) - 70)) (Scale 0.5 0.5 (Color (greyN 0.7) (Text str2)))
    circle = (Color orange (ThickCircle 40 80))
    picm1 = (Translate (100 + x0) (0) circle)
    picm2 = (Translate ((-100) - x0) (0) circle)
    pics1 = drawObj 6 200 (100 + x0) (fromIntegral (-(div y 4))) circle
    pics2 = drawObj 6 (-200) ((-100) - x0) (fromIntegral (div y 4)) circle

drawText :: Int -> Float -> Float -> Float -> Float -> [Int] -> Picture
drawText 1 offset n1 n2 scale (x1: xs) = (Translate (n1 + offset) n2 (Scale scale scale (Color white (Text (show x1)))))
drawText n offset n1 n2 scale (x1 : xs) = Pictures[(Translate (n1 + offset) n2 (Scale scale scale (Color white (Text (show x1))))), (drawText (n-1) offset (n1 + offset) n2 scale xs)]

drawNums :: Mancala -> Picture
drawNums (Mancala s1 m1 s2 m2) = Pictures[pm1, pm2, ps1, ps2, p1, p2]
  where
    pm1 = Translate (-720) (-20) (Scale 0.5 0.5 (Color white (Text (show m1)))) 
    pm2 = Translate (680) (-20) (Scale 0.5 0.5 (Color white (Text (show m2))))
    ps1 = drawText 6 200 (-720) (-220)  0.5 s1
    ps2 = drawText 6 (-200) (680) (180) 0.5 s2
    p1 = drawText 6 200 (-710) (-320) 0.25 [1, 2, 3, 4, 5, 6]
    p2 = drawText 6 (-200) (690) (290) 0.25 [1, 2, 3, 4, 5, 6]

display :: Display 
display = InWindow "Mancala" (x, y) (xShift, yShift)