module Day6 where

import Data.List.Split (splitOn)
import qualified Data.Set as S

-- Star #1

countQ :: String -> Int
countQ = S.size . S.delete '\n' . S.fromList

countQs :: [String] -> Int
countQs = sum . map countQ

-- Star #2

countCQ :: String -> Int
countCQ = S.size . foldr1 S.intersection . map S.fromList . splitOn "\n"

countCQs :: [String] -> Int
countCQs = sum . map countCQ

getSols :: ([String], [String]) -> (String, String)
getSols (inp1, inp2) = (show $ countQs inp1, 
                        show $ countCQs inp2)