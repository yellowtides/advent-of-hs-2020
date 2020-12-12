module Day12 where

import Data.Function (on)

type Vec = (Double, Double)

-- Star #1

(+:+) :: Vec -> Vec -> Vec
(+:+) (i, j) (a, b) = (i+a, j+b)

scale :: Double -> Vec -> Vec
scale k (a, b) = (k*a, k*b)

(-:-) :: Vec -> Vec -> Vec
(-:-) x y = x +:+ scale (-1) y

rotateLeft :: Double -> Vec -> Vec
rotateLeft n (i, j) = let n' = (negate n) * pi / 180 in
                     scale i (cos n', sin n') +:+ scale j (negate $ sin n', cos n')

travel :: [(Char, Double)] -> Vec -> Vec -> Vec
travel [] (i, j) _ = (i, j)
travel ((c, n):xs) pos@(i, j) dir@(i', j')
    | c == 'N' = travel xs (i + n, j) dir 
    | c == 'S' = travel xs (i - n, j) dir
    | c == 'E' = travel xs (i, j + n) dir
    | c == 'W' = travel xs (i, j - n) dir
    | c == 'L' = travel xs pos (rotateLeft n dir)
    | c == 'R' = travel (('L', negate n):xs) pos dir
    | c == 'F' = travel xs (pos +:+ scale n dir) dir

-- Star #2

travel' :: [(Char, Double)] -> Vec -> Vec -> Vec
travel' [] pos _ = pos
travel' ((c, n):xs) pos@(i, j) dir@(i', j')
    | c == 'N' = travel' xs pos (i' + n, j') 
    | c == 'S' = travel' xs pos (i' - n, j')
    | c == 'E' = travel' xs pos (i', j' + n)
    | c == 'W' = travel' xs pos (i', j' - n)
    | c == 'L' = travel' xs pos (rotateLeft n dir)
    | c == 'R' = travel' (('L', negate n):xs) pos dir
    | c == 'F' = travel' xs (pos +:+ scale n dir) dir

manhattan :: Double -> Double -> Double
manhattan  = (+) `on` abs

getSols :: ([(Char, Double)], [(Char, Double)]) -> (String, String)
getSols (inp1, inp2) = (show . uncurry manhattan $ travel inp1 (0, 0) (0, 1),
                        show . uncurry manhattan $ travel' inp2 (0, 0) (1, 10))