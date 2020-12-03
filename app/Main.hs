module Main where

import System.IO
import Data.List.Split (splitOn)
import Control.Monad (unless, fail)

import qualified Day1 as D1 (getSols)
import qualified Day2 as D2 (getSols)

import Utils (parseConstraint)

pad2Dig :: Int -> String
pad2Dig n
    | n < 10    = '0' : show n
    | otherwise = show n

main :: IO ()
main = do
    putStrLn "Please input a day."
    day <- getLine
    let day' = read day
    unless (day' > 0 && day' < 26) 
           (fail "Please input a _valid_ day.")
    file1 <- readFile $ "inputs/star" ++ pad2Dig (day' * 2 - 1) ++ ".txt"
    file2 <- readFile $ "inputs/star" ++ pad2Dig (day' * 2)     ++ ".txt"
    let (star1, star2) = case day' of
            1 -> D1.getSols (map read $ splitOn "\n" file1, 
                             map read $ splitOn "\n" file2)
            2 -> D2.getSols (map parseConstraint $ splitOn "\n" file1,
                             map parseConstraint $ splitOn "\n" file2)
            _ -> error "Patience, fren."
    putStrLn $ "Star #1: " ++ star1
    putStrLn $ "Star #2: " ++ star2
    pure ()