module Main where

import System.IO
import Data.List.Split (splitOn)
import Control.Monad (unless, fail)

import qualified Day01 as D01 (getSols)
import qualified Day02 as D02 (getSols)
import qualified Day03 as D03 (getSols)
import qualified Day04 as D04 (getSols)
import qualified Day05 as D05 (getSols)
import qualified Day06 as D06 (getSols)
import qualified Day07 as D07 (getSols)
import qualified Day08 as D08 (getSols)
import qualified Day09 as D09 (getSols)
import qualified Day10 as D10 (getSols)
import qualified Day11 as D11 (getSols)
import qualified Day12 as D12 (getSols)
import qualified Day13 as D13 (getSols)
import qualified Day14 as D14 (getSols)
import qualified Day15 as D15 (getSols)
import qualified Day16 as D16 (getSols)
import qualified Day17 as D17 (getSols)
import qualified Day18 as D18 (getSols)
import qualified Day19 as D19 (getSols)
import qualified Day20 as D20 (getSols)
import qualified Day21 as D21 (getSols)
import qualified Day22 as D22 (getSols)

import Utils (parseConstraint, parseSackRule, isBetween,
              parseCommand, parseBusInput, parseBusInput2,
              parseSystem, parseTicketDet, parseRex, parseJig,
              parseIngredient, parseGame)

pad2Dig :: Int -> String
pad2Dig n
    | n < 10    = '0' : show n
    | otherwise = show n

main :: IO ()
main = do
    putStrLn "Please input a day."
    day <- getLine
    let day' = read day
    unless (isBetween (1, 25) day') 
           (fail "Please input a _valid_ day.")
    file1 <- readFile $ "inputs/star" ++ pad2Dig (day' * 2 - 1) ++ ".txt"
    file2 <- readFile $ "inputs/star" ++ pad2Dig (day' * 2)     ++ ".txt"
    let (star1, star2) = case day' of
            1 ->  D01.getSols  (map read $ splitOn "\n" file1, 
                               map read $ splitOn "\n" file2)
            2 ->  D02.getSols  (map parseConstraint $ splitOn "\n" file1,
                               map parseConstraint $ splitOn "\n" file2)
            3 ->  D03.getSols  (splitOn "\n" file1, 
                               splitOn "\n" file2)
            4 ->  D04.getSols  (splitOn "\n\n" file1, 
                               splitOn "\n\n" file2)
            5 ->  D05.getSols  (splitOn "\n" file1, 
                               splitOn "\n" file2)
            6 ->  D06.getSols  (splitOn "\n\n" file1, 
                               splitOn "\n\n" file2)
            7 ->  D07.getSols  (map parseSackRule $ splitOn "\n" file1, 
                                map parseSackRule $ splitOn "\n" file2)
            8 ->  D08.getSols  (splitOn "\n" file1,
                                splitOn "\n" file2)
            9 ->  D09.getSols  (map read $ splitOn "\n" file1,
                                map read $ splitOn "\n" file2)
            10 -> D10.getSols (map read $ splitOn "\n" file1,
                               map read $ splitOn "\n" file2)
            11 -> D11.getSols (splitOn "\n" file1,
                               splitOn "\n" file2)
            12 -> D12.getSols (map parseCommand $ splitOn "\n" file1,
                               map parseCommand $ splitOn "\n" file2)
            13 -> D13.getSols (parseBusInput  $ break (== '\n') file1,
                               parseBusInput2 $ break (== '\n') file2)
            14 -> D14.getSols (map parseSystem $ splitOn "\n" file1,
                               map parseSystem $ splitOn "\n" file2)
            15 -> D15.getSols (map read $ splitOn "," file1,
                               map read $ splitOn "," file2)
            16 -> D16.getSols (parseTicketDet file1,
                               parseTicketDet file2)
            17 -> D17.getSols (splitOn "\n" file1,
                               splitOn "\n" file2)
            18 -> D18.getSols (splitOn "\n" file1,
                               splitOn "\n" file2)
            19 -> D19.getSols (parseRex $ splitOn "\n\n" file1,
                               parseRex $ splitOn "\n\n" file2)
            20 -> D20.getSols (parseJig $ splitOn "\n\n" file1,
                               parseJig $ splitOn "\n\n" file2)
            21 -> D21.getSols (map parseIngredient $ splitOn "\n" file1,
                               map parseIngredient $ splitOn "\n" file2)
            22 -> D22.getSols (parseGame file1,
                               parseGame file2)
            _ -> error "Patience, fren."
    putStrLn $ "Star #1: " ++ star1
    putStrLn $ "Star #2: " ++ star2
    pure ()