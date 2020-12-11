module Day03 where

downSlope :: (Int, Int) -> Int -> [String] -> Int
downSlope _      _     [] = 0
downSlope (r, d) start ss = fromEnum (cycle (head ss)!!start == '#') 
                            + downSlope (r, d) (start+r) (drop d ss) 

getSols :: ([String], [String]) -> (String, String)
getSols (inp1, inp2) = (show $ downSlope (3, 1) 0 inp1, 
                        show . product $ fromIntegral <$> (downSlope <$> 
                                         [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] 
                                         <*> pure 0 <*> pure inp2))