module Main (main) where

import Control.Arrow ((>>>))
import System.Environment (getArgs)
import Text.Parsec (parse)

import Sina


main :: IO ()
main = getArgs >>= hArgs


hArgs :: [String] -> IO ()
hArgs = \case
    ("file" : fname : _) -> do
        input <- readFile fname
        pipeline fname input
    ("expr" : expr : _) -> pipeline "stdin" expr
    _ ->
        mapM_
            putStrLn
            [ "Usage: <command> [args]"
            , ""
            , "avaible commands:"
            , "- file PATH\t  parse and tc a file"
            , "- expr EXPR\t  parse and tc a string arg"
            ]


pipeline :: String -> String -> IO ()
pipeline fileName str =
    either print hParse $ parse parseExpr fileName str
  where
    hParse :: Expr -> IO ()
    hParse expr = do
        putStrLn $ "Parsed: " <> ppExpr expr
        putStrLn $ "AST: " <> show expr
        hInfer expr

    hInfer =
        runInfer >>> \case
            Left err -> putStrLn $ "Inference error: " <> err
            Right (ty, n, cs) -> do
                putStrLn $ "Inferred type (unresolved): " <> ppType ty
                putStrLn $ "Fresh variable count: " <> show n
                putStrLn $ "Constraints (" <> show (length cs) <> "):"
                mapM_ (putStrLn . ("  " <>) . ppConstraint) cs
                hSolve ty cs

    hSolve ty =
        runSolve >>> \case
            Left err -> putStrLn $ "Unification error: " <> err
            Right subst -> do
                putStrLn $ "Substitution: " <> ppSubst subst
                putStrLn $ "Final type: " <> ppType (apply subst ty)
