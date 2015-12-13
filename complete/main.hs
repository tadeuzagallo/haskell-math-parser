import System.Environment (getArgs)

import Lexer
import Parser
import Generator
import VM

main :: IO ()
main =
  getArgs >>= (\operations ->
    mapM_ (\op -> printAndSolve $ op) operations
  )

printAndSolve :: String -> IO ()
printAndSolve expr =
  let tac = compile expr
   in (putStrLn . prettyPrintTAC $ tac) >> (print . eval $ tac)

compile :: String -> [TAC]
compile expression =
  generate . parse . tokenize $ expression
