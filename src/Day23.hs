module Day23 where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HM
import Data.Foldable (toList)

-- Star #1

insertAfter :: Int -> [Int] -> [Int] -> [Int]
insertAfter c is []     = []
insertAfter c is (x:xs) 
    | c == x    = x : is ++ xs 
    | otherwise = x : insertAfter c is xs 

playGame :: [Int] -> [Int]
playGame (curr:x:y:z:oth) = let dest    = abs' $ curr - 1
                                abs' n  = if n == 0 then 9 else n
                                dest'   = until (not . (`elem` [x, y, z])) (abs' . subtract 1) dest in
                            insertAfter dest' [x, y, z] oth ++ [curr]

playGameN :: Int -> [Int] -> [Int]
playGameN 0 = id
playGameN i = playGameN (i-1) . playGame

-- Star #2

maxVal :: Int
maxVal = 1000000

playGameSmart :: (Int, HM.HashMap Int Int) -> (Int, HM.HashMap Int Int)
playGameSmart (curr, alles)  = let x      = fromJust $ HM.lookup curr alles
                                   y      = fromJust $ HM.lookup x    alles
                                   z      = fromJust $ HM.lookup y    alles
                                   dest   = abs' $ curr - 1
                                   abs' n = if n == 0 then maxVal else n
                                   dest'  = until (not . (`elem` [x, y, z])) (abs' . subtract 1) dest
                                   afterD = fromJust $ HM.lookup dest' alles
                                   afterZ = fromJust $ HM.lookup z     alles
                                   alles' = HM.insert curr afterZ $! HM.insert dest' x $! HM.insert z afterD alles 
                                   next   = fromJust $ HM.lookup curr alles' in
                               (next, alles')

playGameSmartN :: Int -> (Int, HM.HashMap Int Int) -> HM.HashMap Int Int
playGameSmartN 0 = snd
playGameSmartN i = playGameSmartN (i-1) . playGameSmart

initGame :: [Int] -> (Int, HM.HashMap Int Int)
initGame xs = (head xs', HM.fromList $ (last xs', head xs') : zip xs' (drop 1 xs'))
    where 
        xs' = xs ++ [10..maxVal]

getProd :: HM.HashMap Int Int -> Int
getProd mem = let fs = fromJust $ HM.lookup 1 mem
                  sn = fromJust $ HM.lookup fs mem in
              fs * sn

getSols :: ([Int], [Int]) -> (String, String)
getSols (inp1, inp2) = (concat . reverse . splitOn "1" . concat . map show $ playGameN 100 inp1, 
                        show . getProd . playGameSmartN 10000000 $ initGame inp2)