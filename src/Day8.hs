module Day8 where

import qualified Data.HashSet as HS

type Instr = (String, Int)

toInstr :: String -> Instr
toInstr = fmap (read . stripPlus . tail) . break (== ' ')
        where
            stripPlus ('+':s) = s
            stripPlus s       = s

getAccum :: ([Instr], [Instr]) -> HS.HashSet Int -> Int -> (Bool, Int)
getAccum (_, [])      _ _ = (True, 0)
getAccum (prev, alli@((i, val):is)) exec pc
    | pc `HS.member` exec = (False, 0)
    | i == "acc"          = (val +) <$> getAccum ((i, val):prev, is) newSet (pc+1)
    | i == "jmp"          = if val > 0 
                                then getAccum (reverse (take val alli) ++ prev, drop val alli) newSet (pc+val)
                                else getAccum (drop (abs val) prev, reverse (take (abs val) prev) ++ alli) newSet (pc+val)
    | otherwise           = getAccum ((i, val):prev, is) newSet (pc+1)
    where 
        newSet = HS.insert pc exec

fixInstr :: ([Instr], [Instr]) -> Int
fixInstr (prev, (i, val):is)
    | i `elem` ["jmp", "nop"] = let inv "jmp" = "nop"
                                    inv "nop" = "jmp"
                                    testFix = getAccum ([], reverse prev ++ ((inv i, val):is)) HS.empty 0 in
                                if fst testFix
                                    then snd testFix
                                    else fixInstr ((i, val):prev, is)
    | otherwise  = fixInstr ((i, val):prev, is)

getSols :: ([String], [String]) -> (String, String)
getSols (inp1, inp2) = (show . snd $ getAccum ([], map toInstr inp1) HS.empty 0,
                        show $ fixInstr ([], map toInstr inp2))