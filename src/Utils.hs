module Utils where

import Data.List.Split (splitOn, splitOneOf)
import Data.List (delete, splitAt, inits, tails)
import Data.Char (isDigit)
import Data.Bifunctor (first)
import Data.Array

import qualified Data.Set as S

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

splitAtRev :: Int -> [a] -> ([a], [a])
splitAtRev n = first reverse . splitAt n

subLists :: [a] -> [[a]]
subLists = concat . map tails . inits

many :: Eq a => (a -> a) -> a -> a
many = until =<< ((==) =<<)

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

parseCommand :: String -> (Char, Double)
parseCommand = first head . fmap read . break isDigit

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

-- 2d array stuff

genIndex :: (Int, Int) -> [(Int, Int)]
genIndex (st, fi) = (,) <$> [st..fi] <*> [st..fi]

mkMatrix :: [[a]] -> Array (Int, Int) a
mkMatrix inp = array ((0, 0), (length inp - 1, length inp - 1)) 
               $ zip (genIndex (0, length inp - 1)) (concat inp)

isInBounds :: (Int, Int) -> Array (Int, Int) a -> Bool
isInBounds (i, j) aii = i >= (fst . fst) (bounds aii) && j >= (snd . fst) (bounds aii) 
                     && i <= (fst . snd) (bounds aii) && j <= (snd . snd) (bounds aii)