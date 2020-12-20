module Day20 where

import Data.Array
import Data.List (nub)
import Utils (genIndex)

type Piece = (String, String, String, String)
        --    Top,    Right,  Down,   Left

rotateR :: Piece -> Piece
rotateR (t, r, d, l) = (reverse l, t, reverse r, d)

rotations :: Piece -> [Piece]
rotations = rot' 3
          where
              rot' 0 p = [p]
              rot' i p = p : rot' (i-1) (rotateR p)

flipH :: Piece -> Piece
flipH (t, r, d, l) = (d, reverse r, t, reverse l)

flipV :: Piece -> Piece
flipV (t, r, d, l) = (reverse t, l, reverse d, r)

flips :: Piece -> [Piece]
flips p = [flipH . flipV, flipH, flipV, id] <*> pure p

getPoss :: Piece -> [Piece]
getPoss p = nub $ (concat $ flips <$> rotations p) ++ (concat $ rotations <$> flips p)

-- this piece is matched by all
univPiece :: Piece
univPiece = ("", "", "", "")

-- t = 0, r = 1, d = 2, l = 3
fits :: Int -> Piece -> Piece -> Bool
fits o p1@(t, r, d, l) p2@(t', r', d', l')
    | p1 == univPiece = True     -- p1 is an universal piece, p2 fits! 
    | o == 0          = t == d'  -- p2 fits to the top    of p1
    | o == 1          = r == l'  -- p2 fits to the right  of p1
    | o == 2          = d == t'  -- p2 fits to the bottom of p1
    | o == 3          = l == r'  -- p2 fits to the left   of p1

-- Star #1

type Footage = (Int, Piece)
type Table   = Array (Int, Int) Footage

fitAt :: (Int, Int) -> Footage -> Table -> [Table]
fitAt (i, j) (pid, p) table = let pieceUp   = if i > 0 then snd $ table!(i-1, j) else univPiece
                                  pieceLeft = if j > 0 then snd $ table!(i, j-1) else univPiece
                                  -- make sure the pieces fit to the bottom of pieceUp
                                  -- and to the right of pieceLeft
                                  cands     = getPoss p
                                  goodCands = filter (fits 2 pieceUp) $
                                              filter (fits 1 pieceLeft) cands in 
                                              -- [Piece], all pieces that fit
                              [table // [((i, j), (pid, newP))] | newP <- goodCands]

construct :: (Int, Int) -> Int -> [Footage] -> [Table] -> [Table]
construct (i, j) len fs tables
    | j == len  = if i == len-1 then tables else construct (i+1, 0) len fs tables
    | otherwise = let tables' = concat [fitAt (i, j) f t | t <- tables, f <- fs,
                                                           not $ fst f `elem` map fst (elems t)] in
                  construct (i, j+1) len fs tables'

getIDProd :: Table -> Int
getIDProd t = product [fst $ t ! ij | ij <- (,) <$> [0, len] <*> [0, len]]
         where
             len = snd . snd $ bounds t 

-- Star #2

getSols :: ([(Int, Piece)], [(Int, Piece)]) -> (String, String)
getSols (inp1, inp2) = (show . getIDProd . head $ construct (0,0) len1 inp1 [initTable1],
                        show $ length inp2)
                     where
                         len1       = round . sqrt . fromIntegral $ length inp1
                         initTable1 = array ((0, 0), (len1 - 1, len1 - 1)) 
                                      . zip (genIndex (0, len1 - 1)) $ repeat (-1, univPiece)