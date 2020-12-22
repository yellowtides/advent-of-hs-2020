module Day22 where

import qualified Data.Set      as S
import qualified Data.Sequence as SQ
import Data.Sequence ((|>))
import Data.Foldable (toList)
import Data.Bifunctor (first)

-- Star #1

playGame :: ([Int], [Int]) -> [Int]
playGame ([], ys) = ys
playGame (xs, []) = xs
playGame ((x:xs), (y:ys))
    | x > y     = playGame (xs ++ x:[y], ys)
    | otherwise = playGame (xs, ys ++ y:[x])

-- Star #2

type GameState = (SQ.Seq Int, SQ.Seq Int)

playGame' :: GameState -> S.Set GameState -> (Bool, [Int])
playGame' (xs, ys) mem
    | null xs                            = (True,  toList ys)
    | null ys                            = (False, toList xs)
    | (xs, ys) `S.member` mem            = (False, toList xs)
    | x <= length xs' && y <= length ys' = let recRes = playGame' (SQ.take x xs', SQ.take y ys') S.empty in
                                           if fst recRes
                                                then playGame' (xs', ys' |> y |> x) $! mem'
                                                else playGame' (xs' |> x |> y, ys') $! mem'
    | x > y                              = playGame' (xs' |> x |> y, ys') $! mem'
    | otherwise                          = playGame' (xs', ys' |> y |> x) $! mem'
    where
        x    = xs `SQ.index` 0
        y    = ys `SQ.index` 0
        xs'  = SQ.drop 1 xs
        ys'  = SQ.drop 1 ys
        mem' = S.insert (xs, ys) mem

getSols :: (([Int], [Int]), ([Int], [Int])) -> (String, String)
getSols (inp1, inp2) = (show . sum . map (uncurry (*)) . zip [1..] . reverse . playGame $ inp1,
                        show . sum . map (uncurry (*)) . zip [1..] . reverse . snd $ playGame' (first SQ.fromList $ fmap SQ.fromList inp2) S.empty)