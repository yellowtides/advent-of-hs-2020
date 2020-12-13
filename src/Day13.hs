module Day13 where

import Data.Bifunctor (first)

-- Star #1

firstBus :: Int -> [Int] -> (Int, Int)
firstBus dt = minimum . map (ceiling . (fromIntegral dt /) . fromIntegral >>= (*) >>= (,))

-- Star #2

-- extended euclidean algorithm
eea :: Integer -> Integer -> (Integer, Integer)
eea a 0 = (1, 0)
eea a b = (t, s - q * t)
        where 
            (q, r) = a `quotRem` b
            (s, t) = eea b r

-- always returns a tuple where the right integer is negative
rNegEea :: Integer -> Integer -> (Integer, Integer)
rNegEea a b 
    | k' < 0    = (l', k')
    | otherwise = (l' + b, k' - a) 
    where
        (l', k') = eea a b

-- offset = k * n1 - l * n2, what are the minimal positive integers n1 and n2?
solveOffset :: Integer -> Integer -> Integer -> (Integer, Integer)
solveOffset offset k l = let (n1', n2') = fmap abs $ rNegEea k l
                             lMul  = k * n1' * offset
                             rMul  = l * n2' * offset
                             fact  = lcm k l in
                         (lMul `rem` fact `div` k, rMul `rem` fact `div` l)

-- accumulator for addM should be the left operand, so use foldl!
addM :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addM (o1, m1) (o2, m2) = (o1 + m1 * (snd $ solveOffset (o1+o2) m2 m1), lcm m1 m2)

getSols :: ((Int, [Int]), [(Integer, Integer)]) -> (String, String)
getSols (inp1, inp2) = (show . uncurry (*) . first (subtract (fst inp1)) $ (uncurry firstBus) inp1,
                        show $ foldl1 addM inp2)