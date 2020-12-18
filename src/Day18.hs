module Day18 where

import Data.Char (isDigit, isSpace)
import Utils (many)

data Expr = Pls Expr Expr |
            Tim Expr Expr |
            Val Int       |
            Debug String
            deriving Show

parseExpr :: String -> Expr
parseExpr = parseExprNoWS . fixBrackets . reverse . filter (not . isSpace)
          where
              fixBrackets = map fixBracket
              fixBracket c
                | c == '('  = ')'
                | c == ')'  = '('
                | otherwise = c

parseExprNoWS :: String -> Expr
parseExprNoWS (c:s)
    | all isDigit (c':s') = Val $ read (c':s')
    | op == '+'           = Pls l' r'
    | op == '*'           = Tim l' r'
    | otherwise           = Debug (c':s')
    where
        (c':s')   = many removeB (c:s)
        (l', r')  = (parseExprNoWS l, parseExprNoWS r)
        (l, op:r) = if c' == '('
                        then (maybeL, drop (length maybeL + 1) s')
                        else break (`elem` "+*") (c':s')
        maybeL    = matchNextBracket 0 s'

removeB :: String -> String
removeB (c:s) = if c == '(' && init s == matchNextBracket 0 s
                    then init s
                    else (c:s)

matchNextBracket :: Int -> String -> String
matchNextBracket _ []    = error "mismatched brackets"
matchNextBracket i (x:xs)
    | i == 0 && x == ')' = []
    | x == '('           = x : matchNextBracket (i+1) xs
    | x == ')'           = x : matchNextBracket (i-1) xs
    | otherwise          = x : matchNextBracket i xs

-- Star #1

eval :: Expr -> Int
eval (Pls e1 e2) = eval e1 + eval e2
eval (Tim e1 e2) = eval e1 * eval e2
eval (Val x)     = x

-- Star #2

matchNextBracketRL :: Int -> String -> String
matchNextBracketRL _ []    = error "mismatched brackets rl"
matchNextBracketRL i (x:xs)
    | i == 0 && x == '(' = []
    | x == ')'           = x : matchNextBracketRL (i+1) xs
    | x == '('           = x : matchNextBracketRL (i-1) xs
    | otherwise          = x : matchNextBracketRL i xs

bracketPls :: String -> String
bracketPls = bracketPlsNoWS . filter (not . isSpace)

bracketPlsNoWS :: String -> String
bracketPlsNoWS (s:ss) = bracket (s:[], ss)

bracket :: (String, String) -> String
bracket (alles, [])   = reverse alles
bracket (alles, [ch]) = reverse (ch:alles)
bracket (p:prev, op:f:next)
    | op == '+'  = let l'   = if p == ')' 
                                then ')':matchNextBracketRL 0 prev ++ "("
                                else takeWhile isDigit (p:prev)
                       r'gr = if f == '('
                                then '(':matchNextBracket 0 next ++ ")"
                                else takeWhile isDigit (f:next)
                       r'   = if f == '('
                                then bracketPls r'gr
                                else r'gr
                       gr   = '(':(reverse l') ++ '+':r' ++ ")" in
                    bracket (reverse gr ++ drop (length l')   (p:prev), 
                                           drop (length r'gr) (f:next))
    | otherwise = bracket (op:p:prev, f:next)

getSols :: ([String], [String]) -> (String, String)
getSols (inp1, inp2) = (show . sum $ map (eval . parseExpr) inp1, 
                        show . sum $ map (eval . parseExpr . bracketPls) inp2)