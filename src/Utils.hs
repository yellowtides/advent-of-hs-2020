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

parseBusInput :: (String, String) -> (Int, [Int])
parseBusInput (xS, xsS) = (read xS, map read . filter (/= "x") $ splitOn "," (tail xsS))

parseBusInput2 :: (String, String) -> [(Integer, Integer)]
parseBusInput2 (xS, xsS) = parseBusInput' 0 $ splitOn "," (tail xsS)
                         where
                             parseBusInput' k [] = []
                             parseBusInput' k (x:xs) 
                                | x == "x"  = parseBusInput' (k+1) xs
                                | otherwise = (k, read x) : parseBusInput' (k+1) xs

type Mask     = String
type MemWrite = (Int, Int)

parseSystem :: String -> Either Mask MemWrite
parseSystem s
    | l == "mask" = Left r
    | otherwise   = Right (read . drop 4 $ init l, read r)
    where
        [l, r] = splitOn (" = ") s

type LocRange = (Int, Int)
type Ticket = [Int]

parsef1 :: String -> (LocRange, LocRange)
parsef1 f1 = let [n1, n2, n3, n4] = map read . filter (isDigit . head) $ splitOneOf " -" f1 in
             ((n1, n2), (n3, n4))

parseTicketDet :: String -> ([(LocRange, LocRange)], Ticket, [Ticket])
parseTicketDet inp = let [f1, f2, f3] = splitOn "\n\n" inp in
                     (map parsef1 $ splitOn "\n" f1,
                     map read . splitOn "," . last $ splitOn "\n" f2,
                     map (map read . splitOn ",") . tail $ splitOn "\n" f3)

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

-- 3d array stuff

genIndex3 :: (Int, Int) -> [(Int, Int, Int)]
genIndex3 (st, fi) = (,,) <$> [st..fi] <*> [st..fi] <*> [st..fi] 

mkZeroCube3 :: (Int, Int) -> Array (Int, Int, Int) Char
mkZeroCube3 (lb, ub) = array ((lb, lb, lb), (ub, ub, ub)) 
                       $ zip (genIndex3 (lb, ub)) (cycle ".")

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
    where 
        (as, bs) = splitAt n xs

margin3 :: (Int, Int, Int) -> Array (Int, Int, Int) Char -> Bool
margin3 (i, j, k) aiii 
    = i `elem` allBounds || j `elem` allBounds || k `elem` allBounds 
    where
        ((il, jl, kl), (ir ,jr, kr)) = bounds aiii
        allBounds = [il, jl, kl, ir, jr, kr]
        -- I know that this can be done with concat and uncurrying buuuut :(

-- 4d array stuff let's fucking go

genIndex4 :: (Int, Int) -> [(Int, Int, Int, Int)]
genIndex4 (st, fi) = (,,,) <$> [st..fi] <*> [st..fi] <*> [st..fi] <*> [st..fi] 

mkZeroCube4 :: (Int, Int) -> Array (Int, Int, Int, Int) Char
mkZeroCube4 (lb, ub) = array ((lb, lb, lb, lb), (ub, ub, ub, ub)) 
                       $ zip (genIndex4 (lb, ub)) (cycle ".")

margin4 :: (Int, Int, Int, Int) -> Array (Int, Int, Int, Int) Char -> Bool
margin4 (i, j, k, w) aiii 
    = i `elem` allBounds || j `elem` allBounds || k `elem` allBounds || w `elem` allBounds 
    where
        ((il, jl, kl, wl), (ir ,jr, kr, wr)) = bounds aiii
        allBounds = [il, jl, kl, wl, ir, jr, kr, wr]
        -- I know that this can be done with concat and uncurrying buuuut :(

-- regex stuff

parseRex :: [String] -> ([(Int, String)], [String])
parseRex [rules, els] = (map parseR $ splitOn "\n" rules, splitOn "\n" els)
    where
        parseR = fmap (tail . tail) . first read . break (== ':')