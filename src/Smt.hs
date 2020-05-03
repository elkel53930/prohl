{-# LANGUAGE QuasiQuotes #-}
module Smt where

import Type
import Data.String.Interpolate
import Data.Map as M

cmd :: String
cmd = "(check-sat)\n(get-model)\n"

assert :: String -> String
assert s = "(assert " ++ s ++ ")\n"

declaresToSmt :: [DeclareVar] -> String
declaresToSmt = Prelude.foldl declareToSmt ""

declareToSmt :: String -> DeclareVar -> String
declareToSmt s (SimpleVar v) = [i|#{s}(declare-var #{v} Int)\n|]
declareToSmt s (ArrayVar v) = [i|#{s}(declare-var #{v} (Array Int Int))\n|]

toSmt :: Expression -> String
toSmt (Number a) = show a
toSmt (Boolean b) = if b then "true" else "false"
toSmt (Variable v) = v
toSmt (Select v e) = [i|(select #{v} #{toSmt e})|]
toSmt (Store v e1 e2) = [i|(store #{v} #{toSmt e1} #{toSmt e2})|]
toSmt (BinaryOp op e1 e2) = [i|(#{bopToSmt op} #{toSmt e1} #{toSmt e2})|]
toSmt (UnaryOp Minus e) = [i|(- 0 #{toSmt e})|]
toSmt (UnaryOp Not e) = [i|(not #{toSmt e})|]
toSmt (Paren e) = toSmt e
toSmt (Quantifier q v e) = [i|(#{if q == ALL then "forall" else "exists"} ((#{v} Int)) #{toSmt e})|]


bdict :: M.Map BinaryOperator String
bdict = M.fromList
    [ (Mul, "*"), (Div, "div"), (Mod, "mod"), (Add, "+"), (Sub, "-")
    , (Le, "<="), (Lt, "<"), (Ge, ">="), (Gt, ">"), (Eq, "=")
    , (And, "and"), (Or, "or"), (Imp, "=>")
    ]

bopToSmt :: BinaryOperator -> String
bopToSmt op = s
    where Just s = M.lookup op bdict