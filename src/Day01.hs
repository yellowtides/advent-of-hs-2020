module Day01 where

import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust, isJust, isNothing)
import Data.List (delete)

import Utils (safeHead)

fixExpense :: Int -> [Int] -> Maybe Int
fixExpense n xs = let diffMap = HM.fromListWith (+) $
                                zip xs (repeat (1 :: Int)) in
                  (fmap (uncurry (*)) . safeHead)
                  [(k1, k2) | k1 <- xs,
                              let k2 = n - k1, 
                              HM.lookup k2 (HM.adjust (subtract 1) k1 diffMap) > Just 0]

fixExpense3 :: Int -> [Int] -> Maybe Int
fixExpense3 n xs = (fmap (uncurry (*)) . safeHead)
                   [(k3, fromJust twoProd) 
                      | k3 <- xs,
                        let twoProd = fixExpense (n - k3) (delete k3 xs),
                        isJust twoProd]

getSols :: ([Int], [Int]) -> (String, String)
getSols (inp1, inp2) = (show $ fixExpense 2020 inp1, show $ fixExpense3 2020 inp2)