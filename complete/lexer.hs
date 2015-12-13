module Lexer (
  Token(Number, LParen, RParen, Plus, Minus, Star, Slash),
  tokenize,
) where

import Data.Char (ord, isNumber)

data Token = Number Double | LParen | RParen | Plus | Minus | Star | Slash

tokenize :: String -> [Token]
tokenize expression = reverse tokens where (tokens, _) = tokenize' ([], expression)

tokenize' :: ([Token], String) -> ([Token], String)
tokenize' (tokens, []) = (tokens, [])
tokenize' (tokens, ' ':expr) = tokenize' (tokens, expr)
tokenize' (tokens, lookahead:expr)
  | lookahead == '+' = tokenize' (Plus : tokens, expr)
  | lookahead == '-' = tokenize' (Minus : tokens, expr)
  | lookahead == '*' = tokenize' (Star : tokens, expr)
  | lookahead == '/' = tokenize' (Slash : tokens, expr)
  | lookahead == '(' = tokenize' (LParen : tokens, expr)
  | lookahead == ')' = tokenize' (RParen : tokens, expr)
  | isNumber lookahead =
    let (n, expr') = tokenizeNumber (0, lookahead:expr)
    in tokenize' (Number n : tokens, expr')
  | otherwise = error $ "Unexpected token: '" ++ [lookahead] ++ "'"

tokenizeNumber :: (Double, String) -> (Double, String)
tokenizeNumber (sum, []) = (sum, [])
tokenizeNumber (sum, (lookahead:rest))
  | isNumber lookahead =
    tokenizeNumber ((sum * 10) + (fromIntegral ((ord lookahead) - (ord '0'))), rest)
  | otherwise = (sum, lookahead:rest)

