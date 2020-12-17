module Day17 where

import Data.Array

import Utils (mkZeroCube3, genIndex, margin3, mkZeroCube4, margin4, (|:|))
import Data.List (delete)

-- Star #1

countHash3 :: Array (Int, Int, Int) Char -> Int
countHash3 = length . filter (== '#') . elems

getNeighb3 :: (Int, Int, Int) -> [(Int, Int, Int)] 
getNeighb3 (i, j, k) = delete (i, j, k) $ (,,) <$> [i-1..i+1] <*> [j-1..j+1] <*> [k-1..k+1] 

countLife3 :: (Int, Int, Int) -> Array (Int, Int, Int) Char -> Int
countLife3 (i, j, k) aiii = length . filter isLife $ getNeighb3 (i, j, k)
                         where 
                              isLife ijk = aiii!ijk == '#'

conway3 :: Array (Int, Int, Int) Char  -> Array (Int, Int, Int) Char
conway3 aiii = aiii // [(ijk, c') | ijk <- indices aiii,
                                    not $ margin3 ijk aiii,
                                    (deathp3 ijk |:| birthp3 ijk) aiii,
                                    let c' = if deathp3 ijk aiii then '.' else '#']

deathp3 :: (Int, Int, Int) -> Array (Int, Int, Int) Char -> Bool
deathp3 ijk aiii = (\n -> n /= 2 && n /= 3) (countLife3 ijk aiii) && (aiii!ijk) == '#'

birthp3 :: (Int, Int, Int) -> Array (Int, Int, Int) Char -> Bool 
birthp3 ijk aiii = countLife3 ijk aiii == 3 && (aiii!ijk) == '.'

simulate3 :: Int -> Array (Int, Int, Int) Char -> Array (Int, Int, Int) Char
simulate3 0 xss = xss
simulate3 n xss = simulate3 (n-1) $! conway3 xss

setZero3 :: [[Char]] -> Array (Int, Int, Int) Char -> Array (Int, Int, Int) Char
setZero3 chs aiii = aiii // [((0, j, k), c) | let lb = negate $ length chs `div` 2,
                                              let ub = length chs - abs lb - 1,
                                              ((j, k), c) <- zip (genIndex (lb, ub)) (concat chs)]

-- Star #2

countHash4 :: Array (Int, Int, Int, Int) Char -> Int
countHash4 = length . filter (== '#') . elems

getNeighb4 :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)] 
getNeighb4 (i, j, k, w) = delete (i, j, k, w) $ (,,,) <$> [i-1..i+1] <*> [j-1..j+1] <*> [k-1..k+1] <*> [w-1..w+1]

countLife4 :: (Int, Int, Int, Int) -> Array (Int, Int, Int, Int) Char -> Int
countLife4 (i, j, k, w) aiii = length . filter isLife $ getNeighb4 (i, j, k, w)
                         where 
                              isLife ijkw = aiii!ijkw == '#'

conway4 :: Array (Int, Int, Int, Int) Char  -> Array (Int, Int, Int, Int) Char
conway4 aiii = aiii // [(ijkw, c') | ijkw <- indices aiii,
                                     not $ margin4 ijkw aiii,
                                     (deathp4 ijkw |:| birthp4 ijkw) aiii,
                                     let c' = if deathp4 ijkw aiii then '.' else '#']

deathp4 :: (Int, Int, Int, Int) -> Array (Int, Int, Int, Int) Char -> Bool
deathp4 ijkw aiii = (\n -> n /= 2 && n /= 3) (countLife4 ijkw aiii) && (aiii!ijkw) == '#'

birthp4 :: (Int, Int, Int, Int) -> Array (Int, Int, Int, Int) Char -> Bool 
birthp4 ijkw aiii = countLife4 ijkw aiii == 3 && (aiii!ijkw) == '.'

simulate4 :: Int -> Array (Int, Int, Int, Int) Char -> Array (Int, Int, Int, Int) Char
simulate4 0 xss = xss
simulate4 n xss = simulate4 (n-1) $! conway4 xss

setZero4 :: [[Char]] -> Array (Int, Int, Int, Int) Char -> Array (Int, Int, Int, Int) Char
setZero4 chs aiii = aiii // [((0, 0, j, k), c) | let lb = negate $ length chs `div` 2,
                                                 let ub = length chs - abs lb - 1,
                                                 ((j, k), c) <- zip (genIndex (lb, ub)) (concat chs)]

getSols :: ([[Char]], [[Char]]) -> (String, String)
getSols (inp1, inp2) = (show . countHash3 . simulate3 6 . setZero3 inp1 $ mkZeroCube3 (-10, 10), 
                        show . countHash4 . simulate4 6 . setZero4 inp1 $ mkZeroCube4 (-10, 10))