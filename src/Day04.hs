module Day04 where

import qualified Data.Set as S
import Data.List.Split (splitOneOf)
import Data.List.HT (breakAfter)
import Data.Char (isDigit)

import Utils (isBetween, (&:&))

-- Star #1

noMissingM :: [String] -> Bool
noMissingM = S.isSubsetOf mandPFields . S.fromList . map (takeWhile (/= ':'))

countValidP' :: [[String]] -> Int
countValidP' = sum . map (fromEnum . noMissingM)

-- Star #2

mandPFields :: S.Set String
mandPFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validField :: (String, String) -> Bool
validField (fld, dat) = case fld of
    "byr:" -> and [all isDigit dat,
                   length dat == 4,
                   isBetween (1920, 2002) (read dat :: Int)]
    "iyr:" -> and [all isDigit dat,
                   length dat == 4,
                   isBetween (2010, 2020) (read dat :: Int)]
    "eyr:" -> and [all isDigit dat,
                   length dat == 4,
                   isBetween (2020, 2030) (read dat :: Int)]
    "hgt:" -> let (msr, typ) = break (not . isDigit) dat in
                case typ of
                    "cm" -> and [all isDigit msr,
                                 isBetween (150, 193) (read msr :: Int)]
                    "in" -> and [all isDigit msr,
                                 isBetween (59, 76) (read msr :: Int)]
                    _    -> False
    "hcl:" -> let dat' = drop 1 dat in
                case take 1 dat of
                    "#"  -> and [all (`elem` (['0'..'9'] ++ ['a'..'f'])) dat',
                                 length dat' == 6]
                    _    -> False
    "ecl:" -> dat `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    "pid:" -> and [all isDigit dat,
                   length dat == 9]
    "cid:" -> True
    _      -> False

isValidated :: [String] -> Bool
isValidated = all validField . map (breakAfter (== ':'))

countValidP :: [[String]] -> Int
countValidP = sum . map (fromEnum . (noMissingM &:& isValidated))

getSols :: ([String], [String]) -> (String, String)
getSols (inp1, inp2) = (show . countValidP' $ map (splitOneOf " \n") inp1, 
                        show . countValidP  $ map (splitOneOf " \n") inp2)