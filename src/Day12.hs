module Day12 where

import Linear.V2
import Linear.Matrix ((!*))
import Linear.Vector ((^*), zero, unit)

type Vec = V2 Double

-- Star #1

rotateR :: Double -> Vec -> Vec
rotateR n v = let n' = n * pi / 180 in
              V2 (V2 (cos n') (sin n')) (V2 (negate $ sin n') (cos n')) !* v

rotateL :: Double -> Vec -> Vec
rotateL = rotateR . negate

travel :: [(Char, Double)] -> Vec -> Vec -> Vec
travel [] pos _ = pos
travel ((c, n):xs) pos dir
    | c == 'N' = travel xs (pos + unit _y ^* n) dir 
    | c == 'S' = travel xs (pos - unit _y ^* n) dir
    | c == 'E' = travel xs (pos + unit _x ^* n) dir
    | c == 'W' = travel xs (pos - unit _x ^* n) dir
    | c == 'L' = travel xs pos (rotateL n dir)
    | c == 'R' = travel xs pos (rotateR n dir)
    | c == 'F' = travel xs (pos + dir ^* n) dir

-- Star #2

travel' :: [(Char, Double)] -> Vec -> Vec -> Vec
travel' [] pos _ = pos
travel' ((c, n):xs) pos dir
    | c == 'N' = travel' xs pos (dir + unit _y ^* n)
    | c == 'S' = travel' xs pos (dir - unit _y ^* n)
    | c == 'E' = travel' xs pos (dir + unit _x ^* n)
    | c == 'W' = travel' xs pos (dir - unit _x ^* n)
    | c == 'L' = travel' xs pos (rotateL n dir)
    | c == 'R' = travel' xs pos (rotateR n dir)
    | c == 'F' = travel' xs (pos + dir ^* n) dir

manhattan :: Vec -> Double
manhattan  = sum . fmap abs

getSols :: ([(Char, Double)], [(Char, Double)]) -> (String, String)
getSols (inp1, inp2) = (show . round . manhattan $ travel  inp1 zero (V2 1 0),
                        show . round . manhattan $ travel' inp2 zero (V2 10 1))