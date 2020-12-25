module Day25 where

import Control.Monad (guard)

modVal :: Integer
modVal = 20201227

-- Star #1

bruteSN :: [Integer]
bruteSN = [7]

bruteI :: [Integer]
bruteI = [1..modVal]

modExp :: Integer -> Integer -> Integer
modExp x 0           = 1
modExp x n 
    | n `mod` 2 == 0 = (half * half)     `mod` modVal
    | otherwise      = (x * half * half) `mod` modVal
    where
        half = modExp x (n `div` 2)

findPrivate :: (Integer, Integer) -> Integer
findPrivate (pub1, pub2) = modExp pub2 $ head [i | i <- bruteI, snM <- bruteSN, modExp snM i == pub1]

-- Star #2

getSols :: ((Integer, Integer), (Integer, Integer)) -> (String, String)
getSols (inp1, inp2) = (show $ findPrivate inp1, 
                        show "Free star!")