-- One pass interpreter

import System.Environment (getArgs)

parse :: String -> Double
parse input = output where (output, _) = parseExpr input

parseExpr :: String -> (Double, String)
parseExpr input =
  parseExpr' $ parseTerm input

parseExpr' :: (Double, String) -> (Double, String)
parseExpr' (lhs, input) =
  let operation = apply input lhs parseExpr' parseTerm
  in case input of
    ('+' : _) -> operation (+)
    ('-' : _) -> operation (-)
    _ -> (lhs, input)


parseTerm :: String -> (Double, String)
parseTerm input =
  parseTerm' (parseFactor input)

apply :: String ->
         Double ->
         ((Double, String) -> (Double, String)) ->
         (String -> (Double, String)) ->
         (Double -> Double -> Double) ->
         (Double, String)

apply input lhs complement rhsFn op =
  let (rhs, rest) = rhsFn (drop 1 input)
  in complement ((op lhs rhs), rest) where

parseTerm' :: (Double, String) -> (Double, String)
parseTerm' (lhs, input) =
  let operation = apply input lhs parseTerm' parseFactor
  in case input of
    ('*' : _) -> operation (*)
    ('/' : _) -> operation (/)
    _ -> (lhs, input)

parseFactor :: String -> (Double, String)
parseFactor input = parseFactor' 0 input

parseFactor' :: Double -> String -> (Double, String)
parseFactor' sum [] = (sum, [])
parseFactor' sum (x:xs)
  | x >= '0' && x <= '9' = parseFactor' newSum xs
  where newSum = (sum * 10) + (read [x])::Double

parseFactor' sum ('(':xs) =
  (res, xxs) where (res, (_:xxs)) = parseExpr xs

parseFactor' sum input = (sum, input)

parseOperations :: [String] -> IO ()
parseOperations [] = print "Error: no expression provided"
parseOperations operations =
  mapM_ (print . parse) operations

main :: IO ()
main =
  getArgs >>= (\operations -> parseOperations operations)
