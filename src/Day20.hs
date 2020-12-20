module Day20 where

import Data.Array
import Data.Bifunctor (first)
import Data.Function (on)
import Data.List (nubBy, nub, lookup, transpose)
import Data.Maybe (fromJust)
import Utils (genIndex, groupBy, mkMatrix)
import Text.Regex.PCRE
import qualified Data.Set as S

import Day11 (countHash)

type Mods     = String -- modifiers
type PieceExt = (Mods, Piece)
type Piece    = (String, String, String, String)
            --    Top,    Right,  Down,   Left

rotateR :: PieceExt -> PieceExt
rotateR (mods, (t, r, d, l)) = ('R':mods, (reverse l, t, reverse r, d))

rotations :: PieceExt -> [PieceExt]
rotations = rot' 3
          where
              rot' 0 p = [p]
              rot' i p = p : rot' (i-1) (rotateR p)

flipH :: PieceExt -> PieceExt
flipH (mods, (t, r, d, l)) = ('H':mods, (d, reverse r, t, reverse l))

flipV :: PieceExt -> PieceExt
flipV (mods, (t, r, d, l)) = ('V':mods, (reverse t, l, reverse d, r))

flips :: PieceExt -> [PieceExt]
flips p = [flipH . flipV, flipH, flipV, id] <*> pure p

getPoss :: PieceExt -> [PieceExt]
getPoss p = nubBy ((==) `on` snd) $ (concat $ flips <$> rotations p) 
                                 ++ (concat $ rotations <$> flips p)

-- this piece is matched by all
univPiece :: Piece
univPiece = ("", "", "", "")

-- t = 0, r = 1, d = 2, l = 3
fits :: Int -> Piece -> Piece -> Bool
fits o p1@(t, r, d, l) p2@(t', r', d', l')
    | p1 == univPiece = True     -- p1 is a universal piece, p2 fits! 
    | o == 0          = t == d'  -- p2 fits to the top    of p1
    | o == 1          = r == l'  -- p2 fits to the right  of p1
    | o == 2          = d == t'  -- p2 fits to the bottom of p1
    | o == 3          = l == r'  -- p2 fits to the left   of p1

-- Star #1

type Footage  = (Int, PieceExt)
type Table    = Array (Int, Int) Footage

getPiece :: Footage -> Piece
getPiece = snd . snd

getID :: Footage -> Int
getID = fst

getMods :: Footage -> String
getMods = fst . snd

fitAt :: (Int, Int) -> Footage -> Table -> [Table]
fitAt (i, j) (pid, (m, p)) table = let pieceUp   = if i > 0 then getPiece $ table!(i-1, j) else univPiece
                                       pieceLeft = if j > 0 then getPiece $ table!(i, j-1) else univPiece
                                       -- make sure the pieces fit to the bottom of pieceUp
                                       -- and to the right of pieceLeft
                                       cands     = getPoss (m, p)
                                       goodCands = filter (fits 2 pieceUp   . snd) $
                                                   filter (fits 1 pieceLeft . snd) cands in 
                                                   -- [Piece], all pieces that fit
                                   [table // [((i, j), (pid, newP))] | newP <- goodCands]

construct :: (Int, Int) -> Int -> [Footage] -> [Table] -> [Table]
construct (i, j) len fs tables
    | j == len  = if i == len-1 then tables else construct (i+1, 0) len fs tables
    | otherwise = let tables' = concat [fitAt (i, j) f t | t <- tables, f <- fs,
                                                           not $ getID f `elem` map getID (elems t)] in
                  construct (i, j+1) len fs tables'

-- Star #2 (we assume inp1 == inp2)

getIDProd :: Table -> Int
getIDProd t = product [getID $ t ! ij | ij <- (,) <$> [0, len] <*> [0, len]]
         where
             len = snd . snd $ bounds t 

flipImgV :: [String] -> [String]
flipImgV = map reverse

flipImgH :: [String] -> [String]
flipImgH = reverse

applyMods :: String -> [String] -> [String]
applyMods []   = id
applyMods (m:ms)
    | m == 'R' = applyMods ms . flipImgV . transpose
    | m == 'H' = applyMods ms . flipImgH
    | m == 'V' = applyMods ms . flipImgV

-- this takes an image through all possible orientations
morphsImg :: [[String] -> [String]]
morphsImg = rots ++ [flipImgH] ++ rots ++ [flipImgV] ++ rots ++ [flipImgH] ++ rots
        where
            rots  = applyMods <$> replicate 4 "R"

getBoxes :: [(Int, [String])] -> Table -> [[String]]
getBoxes imgs t = [applyMods (reverse mods) (fromJust $ lookup id imgs) | (id, (mods, _)) <- elems t]

boxToMatrix :: [[String]] -> [String]
boxToMatrix bxs = map concat . concat . map transpose . groupBy (getSideLen bxs) $ map trimEdges bxs

trimEdges :: [String] -> [String]
trimEdges = map (init . tail) . init . tail

type SeaMap = Array (Int, Int) Char

monsterTop, monsterMid, monsterBot :: String
monsterTop = "..................#."
monsterMid = "#....##....##....###"
monsterBot = ".#..#..#..#..#..#..."

getSegment :: SeaMap -> (Int, Int) -> String
getSegment sm (i, j) = [sm!(i, j') | j' <- [j..j-1 + length monsterMid]]

markSeaMonsters :: SeaMap -> SeaMap
markSeaMonsters aii = aii // [((i', j'), 'O') | (i, j) <- indices aii,
                                                i > 0 && i < (snd . snd $ bounds aii),
                                                j-1 + length monsterMid <= (snd . snd $ bounds aii),
                                                getSegment aii (i, j)   =~ monsterMid, 
                                                getSegment aii (i-1, j) =~ monsterTop, 
                                                getSegment aii (i+1, j) =~ monsterBot,
                                                (i', j') <- (,) <$> [i-1..i+1]
                                                                <*> [j..j-1 + length monsterMid],
                                                [monsterTop, monsterMid, monsterBot]!!(i'-i+1)!!(j'-j) == '#']

markAllSeaMonsters :: [String] -> [[String] -> [String]] -> SeaMap
markAllSeaMonsters sm []     = markSeaMonsters $ mkMatrix sm
markAllSeaMonsters sm (f:fs) = let markedMap = markSeaMonsters $ mkMatrix sm
                                   sm'       = groupBy (length sm) $ elems markedMap in
                               markAllSeaMonsters (f sm') fs

getSideLen :: [a] -> Int
getSideLen = round . sqrt . fromIntegral . length

getSols :: ([((Int, Piece), [String])], [((Int, Piece), [String])]) -> (String, String)
getSols (inp1, inp2) = (show . getIDProd $ fitImg,
                        show . countHash $ markAllSeaMonsters seaMapStr morphsImg)
                     where
                         seaMapStr  = boxToMatrix . getBoxes initImgs $ fitImg
                         initImgs   = map (first fst) inp1
                         inp1'      = map fst inp1
                         modInp1    = fmap ((,) "") <$> inp1'
                         len1       = getSideLen inp1'
                         initTable1 = array ((0, 0), (len1 - 1, len1 - 1)) 
                                      . zip (genIndex (0, len1 - 1)) $ repeat (-1, ("", univPiece))
                         fitImg     = head $ construct (0,0) len1 modInp1 [initTable1]