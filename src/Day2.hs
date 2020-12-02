module Day2 where

import Utils (xor, isBetween)

countValid :: [((Int, Int), Char, String)] -> Int
countValid = sum . map (\((lo, hi), ch, pass) -> 
                        fromEnum . isBetween (lo, hi) $ score ch pass) 
            where
                score ch pass = sum $ map (fromEnum . (== ch)) pass

cVIndex :: [((Int, Int), Char, String)] -> Int
cVIndex = sum . map (\((lo, hi), ch, pass) -> 
                     fromEnum $ (pass!!(lo-1) == ch) `xor` (pass!!(hi-1) == ch))

printSols :: ([((Int, Int), Char, String)], 
              [((Int, Int), Char, String)]) -> IO ()
printSols (inp1, inp2) = do
    putStrLn $ "Star #1: " ++ show (countValid inp1)
    putStrLn $ "Star #2: " ++ show (cVIndex inp2)
    pure ()