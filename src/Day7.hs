module Day7 where

import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust, isJust, isNothing)

import Utils (SackRule, SackNum)

-- Part #1

countMemois :: HM.HashMap String [SackNum] -> [String] -> HM.HashMap String Int -> (HM.HashMap String Int, Int)
countMemois dic []                    memois = (memois, 0)
countMemois dic ("shiny gold bag":ss) memois = (fst (countMemois dic ss memois), 1)
countMemois dic (node:rs) memois 
    | node `HM.member` memois = let res = fromJust $ HM.lookup node memois in
                                if res == 1
                                    then (fst (countMemois dic rs memois), 1)
                                    else countMemois dic rs memois
    | otherwise               = let children       = HM.lookup node dic
                                    (newMem, res') = case children of
                                                        Nothing  -> (memois, 0)
                                                        Just chs -> let (mem', res) = countMemois dic (map snd chs) memois in
                                                                    (HM.insert node res mem', res) in
                                countMemois dic (node:rs) newMem

-- Part #2

countBags :: HM.HashMap String [SackNum] -> String -> HM.HashMap String Int -> (HM.HashMap String Int, Int)
countBags dic str memois
    | str `HM.member` memois = (memois, fromJust $ HM.lookup str memois)
    | otherwise              = let children       = HM.lookup str dic
                                   (newMem, res') = case children of
                                                      Nothing  -> (memois, 1)
                                                      Just chs -> foldr (\(o, s) (mem'', v') -> (v' +) <$> 
                                                                            ((o *) <$> countBags dic s mem'')) (memois, 1) chs in
                                (HM.insert str res' newMem, res')

getSols :: ([SackRule], [SackRule]) -> (String, String)
getSols (inp1, inp2) = (show . HM.size . HM.filter (== 1) . fst $ countMemois (HM.fromList inp1) (map fst inp1) HM.empty, 
                        show . subtract 1 . snd $ countBags (HM.fromList inp2) "shiny gold bag" HM.empty)