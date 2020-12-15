module Day15 where

import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)

-- Star #1

playGame' :: Int -> Int -> HM.HashMap Int Int -> [Int]
playGame' n pos mem = n : playGame' (pos - fromMaybe pos (HM.lookup n mem)) (pos+1) (HM.insert n pos mem)

-- play the elves' game, _forever_!
playGame :: [Int] -> [Int]
playGame seed = init seed ++ playGame' (last seed) (length seed) (HM.fromList $ zip (init seed) [1..]) 

-- Star #2

getSols :: ([Int], [Int]) -> (String, String)
getSols (inp1, inp2) = (show $ playGame inp1!!(2020-1),
                        show $ playGame inp2!!(3000000-1))
