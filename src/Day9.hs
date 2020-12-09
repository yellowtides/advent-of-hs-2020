module Day9 where

import qualified Data.HashSet as HS

import Utils (splitAtRev, subLists)

-- Star #1

mkPreamble :: ([Int], [Int]) -> Int -> HS.HashSet Int -> HS.HashSet Int
mkPreamble _              0     currSet = currSet
mkPreamble (_   , [])     pSize currSet = currSet
mkPreamble ([]  , (x:xs)) pSize currSet = mkPreamble ([x], xs) (pSize-1) currSet
mkPreamble (prev, (x:xs)) pSize currSet = mkPreamble (x:prev, xs) (pSize-1) 
                                                     (HS.union (HS.fromList $ map (+ x) prev) currSet)
headInvalid :: ([Int], [Int]) -> Int -> Int
headInvalid (prev, (x:xs)) pSize 
    | not $ HS.member x preamble = x
    | otherwise                  = headInvalid (x:prev, xs) pSize
    where 
        preamble = mkPreamble ([], take pSize prev) pSize HS.empty

getInvalid :: [Int] -> Int -> Int
getInvalid xs pSize = headInvalid (splitAtRev pSize xs) pSize 

-- Star #2

twoSubLists :: [a] -> [[a]]
twoSubLists = filter ((> 1) . length) . subLists

weakSubList :: [Int] -> Int -> [Int]
weakSubList l pSize = let weakNum = getInvalid l pSize in
                      head $ dropWhile ((/= weakNum) . sum) (twoSubLists l)

getSols :: ([Int], [Int]) -> (String, String)
getSols (inp1, inp2) = (show $ getInvalid inp1 25,
                        show . sum $ [maximum, minimum] <*> pure (weakSubList inp1 25))