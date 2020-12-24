module Day24 where

import qualified Data.HashSet as HS
import qualified Data.Set     as S
import Data.Sort (sort)
import Data.Foldable (toList)
import Data.List (union)

-- Star #1

roundN :: Int -> Float -> Float
roundN n f = (fromInteger . round $ f * (10^n)) / (10.0^^n)

hexUnit :: Float
hexUnit = sqrt 0.75

toLoc :: (Float, Float) -> [String] -> (Float, Float)
toLoc (i, j) []     = (roundN 3 i, roundN 3 j) 
toLoc (i, j) (x:xs)
    | x == "e"  = toLoc (i,         j+1)   xs
    | x == "w"  = toLoc (i,         j-1)   xs
    | x == "se" = toLoc (i-hexUnit, j+0.5) xs
    | x == "sw" = toLoc (i-hexUnit, j-0.5) xs
    | x == "ne" = toLoc (i+hexUnit, j+0.5) xs
    | x == "nw" = toLoc (i+hexUnit, j-0.5) xs

addLoc :: (Float, Float) -> HS.HashSet (Float, Float) -> HS.HashSet (Float, Float)
addLoc ij mem
    | ij `HS.member` mem = HS.delete ij mem
    | otherwise          = HS.insert ij mem

-- Star #2

getNeighb :: (Float, Float) -> S.Set (Float, Float)
getNeighb (i, j) = S.fromList $ [toLoc (i, j)] <*> (pure <$> ["e", "w", "se", "sw", "ne", "nw"])

countBlack :: S.Set (Float, Float) -> S.Set (Float, Float) -> Int
countBlack mem = length . S.filter (`S.member` mem)

flipC :: S.Set (Float, Float) -> (Float, Float) -> Bool
flipC mem (i, j) 
    | (i, j) `S.member` mem = coln >  2 || coln == 0
    | otherwise             = coln == 2
    where
        coln = countBlack mem (getNeighb (i, j))

simulate :: S.Set (Float, Float) -> S.Set (Float, Float)
simulate mem = let allNeighb = foldr S.union S.empty $ S.map getNeighb mem
                   whiteFlip = S.filter (flipC mem)  $ S.filter (not . (`S.member` mem)) allNeighb 
                   blackRems = S.filter (not . flipC mem) mem in
               S.union whiteFlip blackRems

simulateN :: Int -> S.Set (Float, Float) -> S.Set (Float, Float)
simulateN 0 = id
simulateN i = simulateN (i-1) . simulate

getSols :: ([[String]], [[String]]) -> (String, String)
getSols (inp1, inp2) = (show . HS.size $ initMap inp1,
                        show . S.size . simulateN 100 . S.fromList . toList $ initMap inp2)
                    where
                        initMap = foldl (\mem x -> addLoc x mem) HS.empty . map (toLoc (0, 0)) 