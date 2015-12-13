module Parser (
  AST(ASTNumber, ASTBinOp),
  parse,
) where

import Lexer

-- Abstract Syntax Tree data
data AST = ASTNumber Double
         | ASTBinOp AST AST Token

parse :: [Token] -> AST
parse tokens =
  ast where (ast, _) = parseExpr tokens

isExpr :: Token -> Bool
isExpr (Plus) = True
isExpr (Minus) = True
isExpr _ = False

isTerm :: Token -> Bool
isTerm (Star) = True
isTerm (Slash) = True
isTerm _ = False

parseExpr :: [Token] -> (AST, [Token])
parseExpr tokens =
  parseExpr' . parseTerm $ tokens

parseExpr' :: (AST, [Token]) -> (AST, [Token])
parseExpr' (lhs, []) = (lhs, [])
parseExpr' (lhs, lookahead:tokens)
  | isExpr(lookahead) =
      let (rhs, tokens') = parseTerm tokens
       in parseExpr' (ASTBinOp lhs rhs lookahead, tokens')
  | otherwise = (lhs, lookahead:tokens)

parseTerm :: [Token] -> (AST, [Token])
parseTerm tokens =
  parseTerm' . parseFactor $ tokens

parseTerm' :: (AST, [Token]) -> (AST, [Token])
parseTerm' (lhs, []) = (lhs, [])

parseTerm' (lhs, lookahead:tokens)
  | isTerm(lookahead) =
      let (rhs, tokens') = parseFactor tokens
       in parseTerm' (ASTBinOp lhs rhs lookahead, tokens')
  | otherwise = (lhs, lookahead:tokens)

parseFactor :: [Token] -> (AST, [Token])
parseFactor (Number a:tokens) = (ASTNumber a, tokens)
parseFactor (LParen:tokens) =
  let (ast, tokens') = parseExpr tokens
   in (ast, drop 1 tokens')
