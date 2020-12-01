module Day1 where

import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)
import Data.List (delete)

import Utils (safeHead)

fixExpense :: Int -> [Int] -> Maybe Int
fixExpense n xs = let diffMap = HM.fromListWith (+) $
                                zip xs (cycle [1 :: Int]) in
                  (fmap (uncurry (*)) . safeHead)
                  [(k1, k2) | k1 <- xs,
                              let k2 = n - k1, 
                              (HM.lookup k2 $ HM.adjust (subtract 1) k1 diffMap) > Just 0]

fixExpense3 :: Int -> [Int] -> Maybe Int
fixExpense3 n xs = (fmap (uncurry (*)) . safeHead)
                   [(k3, fromJust twoProd) 
                      | k3 <- xs,
                        let twoProd = fixExpense (n - k3) (delete k3 xs),
                        twoProd /= Nothing]

printSols :: ([Int], [Int]) -> IO ()
printSols (inp1, inp2) = do
    let sol1 = fixExpense 2020 inp1
    if sol1 == Nothing
        then putStrLn "No solution found for D1S1."
        else (putStrLn . show) sol1

    let sol2 = fixExpense3 2020 inp2
    if sol2 == Nothing
        then putStrLn "No solution found for D1S2."
        else (putStrLn . show) sol2
    pure ()