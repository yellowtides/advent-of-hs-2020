module Day5 where

import Data.Bifunctor (first)
import Data.Sort (sort)

readPassC :: String -> Int
readPassC = foldl1 ((+) . (* 2)) . map (\x -> case x of {'F' -> 1; 'B' -> 0;
                                                         'L' -> 1; 'R' -> 0})

passToLoc :: String -> (Int, Int) -- (row, column)
passToLoc s = let (rowStr, colStr) = break (`elem` "LR") s in
                (127 - readPassC rowStr,
                   7 - readPassC colStr)

getID :: String -> Int
getID = uncurry (+) . first (8 *) . passToLoc

getHighestID :: [String] -> Int
getHighestID = maximum . map getID

findMissing :: [String] -> Int
findMissing ss = let sortedIDs = sort $ map getID ss in
                    sum [head sortedIDs..last sortedIDs] 
                    - sum sortedIDs

getSols :: ([String], [String]) -> (String, String)
getSols (inp1, inp2) = (show $ getHighestID inp1, 
                        show $ findMissing inp2)
