module Day11 where

import Data.Array

import Utils (many)

-- Star #1

countHash :: Array Int (Array Int Char) -> Int
countHash = length . filter (== '#') . concat . map elems . elems

countOccupied :: Array Int (Array Int Char) -> Int -> Int -> Int
countOccupied aii i j = length . filter (== True) $
                                    [i > 0 && j > 0               && aii!(i-1)!(j-1) == '#',
                                    i > 0                         && aii!(i-1)!j     == '#',
                                    i > 0 && j < snd (bounds aii) && aii!(i-1)!(j+1) == '#',
                                    j > 0                         && aii!i    !(j-1) == '#',
                                    j < snd (bounds aii)          && aii!i    !(j+1) == '#',
                                    i < snd (bounds aii) && j > 0 && aii!(i+1)!(j-1) == '#',
                                    i < snd (bounds aii)          && aii!(i+1)!j     == '#',
                                    j < snd (bounds aii) 
                                            && i < snd (bounds aii) && aii!(i+1)!(j+1) == '#']

hashL :: Array Int (Array Int Char)  -> Array Int (Array Int Char)
hashL aii = listArray (bounds aii) [
                listArray (bounds (aii!i)) [
                        if (aii!i!j == 'L')
                            then if countOccupied aii i j == 0
                                    then '#'
                                    else 'L'
                            else aii!i!j 
                    | j <- indices (aii!i)
                ] 
                | i <- indices aii
            ]

hashH :: Array Int (Array Int Char)  -> Array Int (Array Int Char)
hashH aii = listArray (bounds aii) [
                listArray (bounds (aii!i)) [
                        if (aii!i!j == '#')
                            then if countOccupied aii i j >= 4
                                    then 'L'
                                    else '#'
                            else aii!i!j 
                    | j <- indices (aii!i)
                ] 
                | i <- indices aii
            ]

-- Star #2

countOccupiedVis :: Array Int (Array Int Char) -> Int -> Int -> Int
countOccupiedVis aii i j = length . filter (== True) $
                                    [go (i-1) (j-1) (-1) (-1) == '#',
                                     go (i-1)    j  (-1)   0  == '#',
                                     go (i-1) (j+1) (-1)   1  == '#',
                                     go  i    (j-1)   0  (-1) == '#',
                                     go  i    (j+1)   0    1  == '#',
                                     go (i+1) (j-1)   1  (-1) == '#',
                                     go (i+1)    j    1    0  == '#',
                                     go (i+1) (j+1)   1    1  == '#']
                        where
                            go i j diri dirj
                                | i < 0 || j < 0 || i > snd (bounds aii) || j > snd (bounds aii) = '.'
                                | aii!i!j == '#' = '#'
                                | aii!i!j == 'L' = 'L'
                                | otherwise      = go (i+diri) (j+dirj) diri dirj

hashL' :: Array Int (Array Int Char)  -> Array Int (Array Int Char)
hashL' aii = listArray (bounds aii) [
                listArray (bounds (aii!i)) [
                        if (aii!i!j == 'L')
                            then if countOccupiedVis aii i j == 0
                                    then '#'
                                    else 'L'
                            else aii!i!j 
                    | j <- indices (aii!i)
                ] 
                | i <- indices aii
            ]

hashH' :: Array Int (Array Int Char)  -> Array Int (Array Int Char)
hashH' aii = listArray (bounds aii) [
                listArray (bounds (aii!i)) [
                        if (aii!i!j == '#')
                            then if countOccupiedVis aii i j >= 5
                                    then 'L'
                                    else '#'
                            else aii!i!j 
                    | j <- indices (aii!i)
                ] 
                | i <- indices aii
            ]

getSols :: ([String], [String]) -> (String, String)
getSols (inp1, inp2) = (show . countHash $ many (hashH . hashL) 
                                                (listArray (0, length inp1 - 1) $
                                                    map (listArray (0, length inp1 - 1)) inp1),
                        show . countHash $ many (hashH' . hashL') 
                                                (listArray (0, length inp2 - 1) $
                                                    map (listArray (0, length inp2 - 1)) inp2))