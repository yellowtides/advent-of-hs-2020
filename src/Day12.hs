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

rotateDeg :: Double -> Vec -> Vec
rotateDeg n (i, j) = let n' = (negate n) * pi / 180 in
                     (i * cos n' - j * sin n',
                      i * sin n' + j * cos n')

process :: [(Char, Double)] -> Vec -> Vec -> Vec
process [] (i, j) _ = (i, j)
process ((c, n):xs) pos@(i, j) dir@(i', j')
    | c == 'N' = process xs (i + n, j) dir 
    | c == 'S' = process xs (i - n, j) dir
    | c == 'E' = process xs (i, j + n) dir
    | c == 'W' = process xs (i, j - n) dir
    | c == 'L' = process xs pos (rotateDeg n dir)
    | c == 'R' = process (('L', negate n):xs) pos dir
    | c == 'F' = process xs (pos +:+ scale n dir) dir

-- Star #2

processW :: [(Char, Double)] -> Vec -> Vec -> Vec
processW [] pos _ = pos
processW ((c, n):xs) pos@(i, j) wayp@(i', j')
    | c == 'N' = processW xs pos (i' + n, j') 
    | c == 'S' = processW xs pos (i' - n, j')
    | c == 'E' = processW xs pos (i', j' + n)
    | c == 'W' = processW xs pos (i', j' - n)
    | c == 'L' = processW xs pos (pos +:+ rotateDeg n (wayp -:- pos))
    | c == 'R' = processW (('L', negate n):xs) (i, j) wayp
    | c == 'F' = processW xs (pos +:+ scale n (wayp -:- pos)) (wayp +:+ scale n (wayp -:- pos))

manhattan :: Double -> Double -> Double
manhattan  = (+) `on` abs

getSols :: ([(Char, Double)], [(Char, Double)]) -> (String, String)
getSols (inp1, inp2) = (show . uncurry manhattan $ process inp1 (0, 0) (0, 1),
                        show . uncurry manhattan $ processW inp2 (0, 0) (1, 10))