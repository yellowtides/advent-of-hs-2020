module Day21 where

import Data.List (intersect, union, (\\), delete, intercalate)
import Data.Tuple (swap)
import Data.Sort (sortOn, sort)

-- Star #1

type Allergen    = String
type Allergens   = [Allergen]
type Ingredient  = String
type Ingredients = [Ingredient]

allergenCand :: Allergen -> [(Allergens, Ingredients)] -> Ingredients -> (Allergen, Ingredients)
allergenCand a [] cands = (a, cands)  
allergenCand a ((as, is):ais) cands
    | a `elem` as = allergenCand a ais (cands `intersect` is)
    | otherwise   = allergenCand a ais cands

getaAllIngr :: [(Ingredients, Allergens)] -> Ingredients
getaAllIngr = foldr1 union . map fst

getAllAll :: [(Ingredients, Allergens)] -> Allergens
getAllAll = foldr1 union . map snd

possibleAll :: [(Ingredients, Allergens)] -> Ingredients
possibleAll ias = foldr1 union . map (\x -> snd $ allergenCand x (map swap ias) (getaAllIngr ias)) $ getAllAll ias

-- Star #2

scuffedDPLL :: [(Allergen, Ingredients)] -> [(Allergen, Ingredient)]
scuffedDPLL []         = []
scuffedDPLL mem
    | length curr /= 1 = []
    | otherwise        = (tag, find) : scuffedDPLL (map (fmap (delete find)) ms)
    where
        ((tag, curr):ms) = sortOn (length . snd) mem
        find             = head curr

allergenMap :: [(Ingredients, Allergens)] -> [(Allergen, Ingredients)]
allergenMap ias = map (\x -> allergenCand x (map swap ias) (getaAllIngr ias)) $ getAllAll ias

getSols :: ([(Ingredients, Allergens)], 
            [(Ingredients, Allergens)]) -> (String, String)
getSols (inp1, inp2) = (show $ length [i| i <- concat $ map fst inp1, not $ i `elem` (possibleAll inp1)],
                        show . intercalate "," . map snd . sort . scuffedDPLL $ allergenMap inp2)