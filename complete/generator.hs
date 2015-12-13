module Generator (
  TAC(TAC),
  Operation(Mov, Add, Sub, Mul, Div),
  Operator(Register, Value),

  generate,
  prettyPrintTAC,
) where

import Lexer
import Parser

-- Three Address Code data
data TAC = TAC Operation Operator Operator
data Operation = Mov | Add | Sub | Mul | Div
data Operator = Register Int | Value Double

instance Show TAC where
  show (TAC op reg1 reg2) = show op ++ " " ++ show reg1 ++ ", " ++ show reg2

instance Show Operation where
  show Mov = "mov"
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show Div = "div"

instance Show Operator where
  show (Register n) = "%r" ++ show n
  show (Value n) = "$" ++ show n

generate :: AST -> [TAC]
generate ast =
  reverse tac where (tac, _) = generate' ([], ast, Register (-1))

generate' :: ([TAC], AST, Operator) -> ([TAC], Operator)
generate' (tac, ast, (Register reg)) =
  case ast of
    (ASTNumber n) -> (TAC Mov (Value n) (Register (reg + 1)) : tac, (Register (reg + 1)))
    (ASTBinOp lhs rhs op) ->
      let (tac', lhs') = generate' (tac, lhs, Register reg)
       in let (tac'', rhs') = generate' (tac', rhs, lhs')
           in (TAC (opFor op) rhs' lhs': tac'', lhs')

opFor :: Token -> Operation
opFor Plus = Add
opFor Minus = Sub
opFor Star = Mul
opFor Slash = Div

prettyPrintTAC :: [TAC] -> String
prettyPrintTAC tac =
  unlines $ show <$> tac
