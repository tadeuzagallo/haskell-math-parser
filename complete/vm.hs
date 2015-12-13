module VM (
  eval,
) where

import Generator

eval :: [TAC] -> Double
eval tac = result where ((result:_), _) = eval' ([], tac)

eval' :: ([Double], [TAC]) -> ([Double], [TAC])
eval' (reg, []) = (reg, [])
eval' (reg, tac) =
  let ((TAC op _ _):_) = tac
   in case op of
        Mov -> eval' $ mov reg tac
        Add -> eval' $ apply (+) reg tac
        Sub -> eval' $ apply (-) reg tac
        Mul -> eval' $ apply (*) reg tac
        Div -> eval' $ apply (/) reg tac

apply :: (Double -> Double -> Double) -> [Double] -> [TAC] -> ([Double], [TAC])
apply op reg ((TAC _ src dst):tac) =
  let a = readOp src reg
      b = readOp dst reg
   in let reg' = store (op b a) dst reg
       in (reg', tac)

mov :: [Double] -> [TAC] -> ([Double], [TAC])
mov reg ((TAC _ src dst):tac) =
  let value = readOp src reg
   in let reg' = store value dst reg
       in (reg', tac)

readOp :: Operator -> [Double] -> Double
readOp op reg =
  case op of
    (Value n) -> n
    (Register r) -> reg !! r

store :: Double -> Operator -> [Double] -> [Double]
store value (Register reg) regs
  | reg == length regs = regs ++ [value]
  | otherwise =
    let (left, (_:right)) = splitAt reg regs
     in left ++ [value] ++ right
