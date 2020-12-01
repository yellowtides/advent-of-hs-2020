module Main where

import System.IO
import Data.List.Split (splitOn)
import Control.Monad (when, fail)
import qualified Day1 as D1 (printSols)

pad2Dig :: Int -> String
pad2Dig n
    | n < 10    = '0' : show n
    | otherwise = show n

main :: IO ()
main = do
    putStrLn "Please input a day."
    day <- getLine
    let day' = read day
    when (not $ day' > 0 && day' < 26) 
         (fail "Please input a _valid_ day.")
    file1 <- readFile $ "inputs/star" ++ pad2Dig (day' * 2 - 1) ++ ".txt"
    file2 <- readFile $ "inputs/star" ++ pad2Dig (day' * 2)     ++ ".txt"
    case day' of
        1         -> D1.printSols (map read $ splitOn "\n" file1, 
                                   map read $ splitOn "\n" file2)
        2         -> putStrLn "Patience, fren."
        otherwise -> putStrLn "Patience, fren."
    pure ()