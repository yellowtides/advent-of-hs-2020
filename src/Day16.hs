module Day16 where

import qualified Data.HashMap.Lazy as HM
import Data.Tuple.Extra
import Data.Sort (sortOn)
import Data.List (delete)

import Utils (LocRange, Ticket, isBetween, (|:|), many)

-- Star #1

isPossValid :: (LocRange, LocRange) -> Int -> Bool
isPossValid (r1, r2) = isBetween r1 |:| isBetween r2 

isPossValidAll :: [(LocRange, LocRange)] -> Int -> Bool
isPossValidAll rules x = foldl (\acc r -> isPossValid r x || acc) False rules

scanError :: [(LocRange, LocRange)] -> [Int] -> Int
scanError rules = sum . filter (not . isPossValidAll rules) 

-- Star #2

pruneOptions :: HM.HashMap Int [Int] -> [(LocRange, LocRange)] -> (Int, Int) -> HM.HashMap Int [Int]
pruneOptions cand rules (pos, x)
    = foldl (\mem (pos', i') -> HM.adjust (delete i') pos mem) cand 
      $ [(pos, i) | (i, r) <- zip [0..] rules, not $ isPossValid r x]

decipher :: HM.HashMap Int [Int] -> [(LocRange, LocRange)] -> [(Int, Int)] -> HM.HashMap Int [Int]
decipher mem rules pxs = foldl (\mem px -> pruneOptions mem rules px) mem pxs

findPairs :: [(Int, [Int])] -> [(Int, Int)]
findPairs []                           = []
findPairs ((f, (wtfisthiscode:es)):ps) = (f, wtfisthiscode) : findPairs (map (fmap (delete wtfisthiscode)) ps)

getSols :: (([(LocRange, LocRange)], Ticket, [Ticket]), 
            ([(LocRange, LocRange)], Ticket, [Ticket])) -> (String, String)
getSols (inp1, inp2) = (show $ scanError (fst3 inp1) (concat $ thd3 inp1),
                        show . product . map (\(a, _) -> snd3 inp2!!a) . take 6 . sortOn (\(_, b) -> b) .
                               findPairs . sortOn (\(_, xs) -> length xs) . HM.toList $
                               decipher (HM.fromList $ zipWith (,) [0..length (snd3 inp2) - 1] (repeat [0..length (snd3 inp2) - 1]))
                                        (fst3 inp2)
                                        (concat . map (zip [0..]) $ filter (all (isPossValidAll (fst3 inp2))) (thd3 inp2)))