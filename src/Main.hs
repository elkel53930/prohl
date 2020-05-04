module Main where

import Type
import Parser
import Wp
import Smt
import qualified System.Process as C
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0 then
        putStrLn "no filename"
    else do
        code <- readFile $ head args
        let hoare = parseHoare code
        case hoare of
            Right h@(Hoare dv _ _ _) ->do
                mapM_ (z3 dv) $ mkWpExpr h
            Left a -> putStrLn $ show a 

mkWpExpr :: Hoare -> [Expression]
mkWpExpr (Hoare _ pre prog post) = mkImp pre weak_pre : props'
    where
        (props', weak_pre) = wp [] prog post

z3 :: [DeclareVar] -> Expression -> IO()
z3 dv prop = do
    let smt2 = toSmt $ mkNot prop
    putStrLn "\n = = = = = = = = = = =\n"
    putStrLn smt2

    writeFile "prohl.smt2" $ (declaresToSmt dv) ++ assert smt2 ++ cmd
    C.system "z3 prohl.smt2 > prohl_res"
    res <- readFile "prohl_res"
    if "unsat" == (head $ lines res) then
        putStrLn "===> OK"
    else
        do
            C.system "cat prohl_res"
            return ()

