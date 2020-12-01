module Utils where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs