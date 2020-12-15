module Day15 where

import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)

-- Star #1

playGame' :: Int -> Int -> HM.HashMap Int Int -> Int -> Int
playGame' n pos mem goal | pos == goal = n
                         | otherwise   = playGame' (pos - fromMaybe pos (HM.lookup n mem)) (pos+1) (HM.insert n pos mem) goal

-- play the elves' game til the goal is hit!
playGame :: [Int] -> Int -> Int
playGame seed = playGame' (last seed) (length seed) (HM.fromList $ zip (init seed) [1..])

-- Star #2

getSols :: ([Int], [Int]) -> (String, String)
getSols (inp1, inp2) = (show $ playGame inp1 2020,
                        show $ playGame inp2 30000000)