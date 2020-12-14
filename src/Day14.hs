module Day14 where

import qualified Data.HashMap.Lazy as HM
import Data.Bits

import Utils (Mask, MemWrite)

-- Star #1

maskInt :: String -> Int -> Int
maskInt s i = maskInt' 1 (reverse s) i
        where
            maskInt' _    []     val = val
            maskInt' pow2 (m:ms) val 
                | m == '0'  = maskInt' (pow2*2) ms ((val .|. pow2) - pow2)
                | m == '1'  = maskInt' (pow2*2) ms (val .|. pow2)
                | otherwise = maskInt' (pow2*2) ms val

process :: [Either Mask MemWrite] -> String -> HM.HashMap Int Int -> HM.HashMap Int Int 
process []     _    mem = mem
process (x:xs) mask mem = case x of
                            Left mask'       -> process xs mask' mem
                            Right (ind, val) -> process xs mask  $ HM.insert ind (maskInt mask val) mem

-- Star #2
process' :: [Either Mask MemWrite] -> String -> HM.HashMap Int Int -> HM.HashMap Int Int 
process' []     _    mem = mem
process' (x:xs) mask mem = case x of
                             Left mask'       -> process' xs mask' mem
                             Right (ind, val) -> process' xs mask $
                                                 foldl (\mem' m' -> HM.insert (maskInt m' ind) val mem') mem $
                                                 (reverse $ getMasks mask)

getMasks :: String -> [String]
getMasks []     = [""]
getMasks (x:xs)
    | x == '1'  = ('1' :) <$> getMasks xs
    | x == '.'  = ('0' :) <$> getMasks xs
    | x == 'X'  = concat $ getMasks <$> ((:) <$> "1." <*> pure xs)
    | otherwise = ('X' :) <$> getMasks xs

getSols :: ([Either Mask MemWrite], [Either Mask MemWrite]) -> (String, String)
getSols (inp1, inp2) = (show . HM.foldr (+) 0 $ process  inp1 [] HM.empty,
                        show . HM.foldr (+) 0 $ process' inp2 [] HM.empty)

