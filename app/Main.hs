module Main (main) where

import Control.Arrow ((>>>))
import Text.Parsec (parse)

import Sina


main :: IO ()
main = do
    let inputFile = "sample.sina"
    input <- readFile inputFile
    either print hParse $ parse parseExpr inputFile input
  where
    hParse :: Expr -> IO ()
    hParse expr = do
        putStrLn $ "Parsed: " <> ppExpr expr
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
