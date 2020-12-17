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
isPossValidAll rules x = or $ (`isPossValid` x) <$> rules

scanError :: [(LocRange, LocRange)] -> [Int] -> Int
scanError rules = sum . filter (not . isPossValidAll rules) 

-- Star #2

pruneOptions :: HM.HashMap Int [Int] -> [(LocRange, LocRange)] -> (Int, Int) -> HM.HashMap Int [Int]
pruneOptions cand rules (pos, x) = foldr (\i' -> HM.adjust (delete i') pos) cand 
                                   $ [i | (i, r) <- zip [0..] rules, not $ isPossValid r x]

decipher :: HM.HashMap Int [Int] -> [(LocRange, LocRange)] -> [(Int, Int)] -> HM.HashMap Int [Int]
decipher mem rules = foldl (`pruneOptions` rules) mem

findPairs :: [(Int, [Int])] -> [(Int, Int)]
findPairs []             = []
findPairs ((f, x:es):ps) = (f, x) : findPairs (map (fmap $ delete x) ps)

getSols :: (([(LocRange, LocRange)], Ticket, [Ticket]), 
            ([(LocRange, LocRange)], Ticket, [Ticket])) -> (String, String)
getSols (inp1, inp2) = (show $ scanError rules1 (concat othTickets1),
                        show . product . map ((!!) myTicket2 . fst) . take 6 . sortOn snd .
                               findPairs . sortOn (length . snd) . HM.toList $
                               decipher (HM.fromList $ zip initPossib (repeat initPossib)) rules2
                                        (concatMap (zip [0..]) $ filter (all $ isPossValidAll rules2) othTickets2))
                    where
                      (rules1, myTicket1, othTickets1) = inp1
                      (rules2, myTicket2, othTickets2) = inp2
                      initPossib                       = [0..length myTicket2 - 1]