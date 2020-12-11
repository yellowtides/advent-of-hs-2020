module Day11 where

import Data.Array
import Data.List (delete)

import Utils (many, mkMatrix, genIndex, isInBounds)

-- Star #1

countHash :: Array (Int, Int) Char -> Int
countHash = length . filter (== '#') . elems

getNeighb :: (Int, Int) -> [(Int, Int)] 
getNeighb (i, j) = delete (i, j) $ (,) <$> [i-1..i+1] <*> [j-1..j+1]

countOccupied :: (Int, Int) -> Array (Int, Int) Char -> Int
countOccupied (i, j) aii = length . filter (== True) . map safeIndex $ getNeighb  (i, j)
                      where 
                          safeIndex (i, j) = isInBounds (i, j) aii && aii!(i, j) == '#'

changeOn :: ((Int, Int) -> Array (Int, Int) Char -> Bool) ->
                Char -> Array (Int, Int) Char  -> Array (Int, Int) Char
changeOn p c aii = aii // [((i, j), c') | (i, j) <- indices aii,
                                          aii!(i, j) == c, 
                                          p (i, j) aii]
                 where
                     c' = if c == '#' then 'L' else '#'

hashL :: Array (Int, Int) Char  -> Array (Int, Int) Char
hashL = changeOn (\ij -> (== 0) . countOccupied ij) 'L'

hashH :: Array (Int, Int) Char  -> Array (Int, Int) Char
hashH = changeOn (\ij -> (>= 4) . countOccupied ij) '#'

-- Star #2

countOccupiedVis :: (Int, Int) -> Array (Int, Int) Char -> Int
countOccupiedVis (i, j) aii = length . filter (== True) . map isOccup $ getNeighb (i, j)
                        where
                            isOccup (i', j') = go' (i', j') (i' - i, j' -j) == '#'
                            go' (i, j) (diri, dirj)
                                | not $ isInBounds (i, j) aii = '.'
                                | aii!(i, j) == '#'           = '#'
                                | aii!(i, j) == 'L'           = 'L'
                                | otherwise                   = go' (i+diri, j+dirj) (diri, dirj)

hashL' :: Array (Int, Int) Char  -> Array (Int, Int) Char
hashL' = changeOn (\ij -> (== 0) . countOccupiedVis ij) 'L'

hashH' :: Array (Int, Int) Char  -> Array (Int, Int) Char
hashH' = changeOn (\ij -> (>= 5) . countOccupiedVis ij) '#'

getSols :: ([String], [String]) -> (String, String)
getSols (inp1, inp2) = (show . countHash $ many (hashH . hashL) (mkMatrix inp1),
                        show . countHash $ many (hashH' . hashL') (mkMatrix inp2))