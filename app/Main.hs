module Main where

import System.IO
import Data.List.Split (splitOn)
import Control.Monad (guard)
import qualified Day1 as D1 (printsols)

pad2Dig :: Int -> String
pad2Dig n
    | n < 10    = '0' : show n
    | otherwise = show n

main :: IO ()
main = do
    putStrLn "Please input a day."
    day <- getLine
    let day' = read day
    guard $ day' > 0 && day' < 50
    file1 <- readFile $ "inputs/star" ++ pad2Dig (day' * 2 - 1) ++ ".txt"
    file2 <- readFile $ "inputs/star" ++ pad2Dig (day' * 2)     ++ ".txt"
    let (inps1, inps2) = (splitOn "\n" file1, splitOn "\n" file2)
    case day' of
        1         -> D1.printsols (map read inps1) (map read inps2)
        otherwise -> putStrLn "Please input a _valid_ day."
    pure ()