module Utils where

import Data.List.Split (splitOn, splitOneOf)
import Data.List (delete)
import Data.Char (isDigit)

import qualified Data.Set as S

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

parseConstraint :: String -> ((Int, Int), Char, String)
parseConstraint s = let (lo, s1) = break (== '-') s
                        (hi, s2) = break (== ' ') (drop 1 s1)
                        (ch, s3) = break (== ':') (drop 1 s2)
                        s'       = drop 2 s3 in
                    ((read lo, read hi), head ch, s')

type SackNum  = (Int, String)
type SackRule = (String, [SackNum])

parseSackRule :: String -> SackRule
parseSackRule s = let [f, css] = splitOn "s contain" s 
                      cs       = map tail . init $ splitOneOf ",." css
                      csParsed = delete (0, "") $ map parseSackNum cs in
                  (f, csParsed)

parseSackNum :: String -> SackNum
parseSackNum s 
    | take 2 s == "no" = (0, "")
    | otherwise        = let cs    = drop 1 $ dropWhile isDigit s 
                             count = read $ takeWhile isDigit s in
                         (count, if last cs == 's' then init cs else cs)

xor :: Bool -> Bool -> Bool
xor True  False = True
xor False True  = True
xor _     _     = False

isBetween :: (Num a, Ord a) => (a, a) -> a -> Bool
isBetween (a, b) x = a <= x && x <= b

-- predicate conjunction
(&:&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&:&) p1 p2 = \t -> p1 t && p2 t

-- predicate disjunction
(|:|) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|:|) p1 p2 = \t -> p1 t || p2 t