module Main where

import Type
import Parser
import Wp
import Smt

main :: IO ()
main = do
    code <- readFile "min.phl"
    let hoare = parseHoare code
    putStrLn . show $ hoare
    case hoare of
        Right h@(Hoare dv _ _ _) ->do
            putStrLn $ (declaresToSmt dv) ++ (assert . toSmt $ mkWpExpr h) ++ cmd
        Left a -> putStrLn $ show a 

mkWpExpr :: Hoare -> Expression
mkWpExpr (Hoare _ pre prog post) =  mkNot . mkImp pre $ wp prog post

mkNot :: Expression -> Expression
mkNot e = UnaryOp Not e

mkImp :: Expression -> Expression -> Expression
mkImp e1 e2 = BinaryOp Imp e1 e2