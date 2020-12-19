module Day19 where

import Text.Regex.PCRE
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)
import Data.Char (isDigit, isSpace)
import Data.List (lookup)

-- Star #1

mkRex :: Int -> [(Int, String)] -> String
mkRex i s = snd $ mkRex' i (fromJust $ lookup i s) HM.empty (HM.fromList s)
    where
        mkRex' :: Int -> String -> HM.HashMap Int String -> HM.HashMap Int String -> (HM.HashMap Int String, String)
        mkRex' pos s mem rules 
            | head s == '"'       = (mem, init $ tail s)
            | pos `HM.member` mem = (mem, fromJust $ HM.lookup pos mem)
            | otherwise           = let rex      = lb ++ lb' ++ newS s mem ++ rb
                                        (lb, rb) = if '|' `elem` s then ("(", ")") else ("", "")
                                        lb'      = if '+' `elem` s then "("        else "" in
                                    (HM.insert pos rex mem, rex)
            where
                newS [] _ = []
                newS s mem' 
                    | head s == '|' =    ')':'|':'(':newS (drop 2 s) mem'
                    | head s == '+' =        ')':'+':newS (drop 2 s) mem'
                    | otherwise     = lb ++ curr ++ rb ++ newS (drop (length nums' + 1) s) mem''
                    where
                        (lb, rb)      = if length curr > 1 then ("(", ")") else ("", "")
                        nums'         = takeWhile isDigit s
                        nums          = read $ nums'
                        (mem'', curr) = mkRex' nums (fromJust $ HM.lookup nums rules) mem' rules

-- Star #2

getSols :: (([(Int, String)], [String]), 
            ([(Int, String)], [String])) -> (String, String)
getSols (inp1, inp2) = (show . length $ filter (=~ re0inp1) (snd inp1), 
                        show . length $ filter (\s -> any (s =~) re0inp2) (snd inp2))
                    where
                        re0inp1  = '^':mkRex  0 (fst inp1)++"$"
                        re8inp2  = '(':mkRex  8 (fst inp2)++")"
                        re42inp2 = '(':mkRex 42 (fst inp2)++")"
                        re31inp2 = '(':mkRex 31 (fst inp2)++")"
                        re11inp2 = take 7 [concat (replicate i re42inp2) ++
                                           concat (replicate i re31inp2) | i <- [1..]]
                        re0inp2  = map (('^':re8inp2) ++) $ map (++ "$") re11inp2