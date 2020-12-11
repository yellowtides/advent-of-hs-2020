module Day10 where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.Sort (sort)
import Data.Maybe (fromJust)

-- Star #1

getDiffC :: Int -> [Int] -> Int
getDiffC n xs = length . filter (== n) . fmap (uncurry subtract) . zip xs $ drop 1 xs

getF :: [Int] -> Int
getF = (+ 3) . maximum

addSF :: [Int] -> [Int]
addSF xs = 0 : getF xs : xs

-- Star #2

countHash :: HS.HashSet Int -> Int -> HM.HashMap Int Int -> (Int, HM.HashMap Int Int)
countHash dic n hashm
    | n == 0                   = (1, hashm)
    | not (n `HS.member` dic)  = (0, hashm)
    | n `HM.member` hashm      = (fromJust $ HM.lookup n hashm, hashm)
    | otherwise                = let (v',  map')  = countHash dic (n-3) hashm
                                     (v'', map'') = countHash dic (n-2) map'
                                     (vf,  mapf)  = countHash dic (n-1) map'' 
                                     insval = v' + v'' + vf         in
                                 (insval, HM.insert n insval mapf)

getSols :: ([Int], [Int]) -> (String, String)
getSols (inp1, inp2) = (show . product $ getDiffC <$> [3, 1] <*> [sort $ addSF inp1],
                        show . fst $ countHash (HS.fromList $ addSF inp2) (getF inp2) HM.empty)