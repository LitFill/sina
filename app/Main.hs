module Main (main) where

import Text.Parsec (parse)

import Sina


main :: IO ()
main = do
    let inputFile = "sample.sina"
    input <- readFile inputFile
    either putStrLn hr $ mapLeft show $ parse parseExpr inputFile input
  where
    hr :: Expr -> IO ()
    hr expr = do
        putStrLn $ "Parsed: " <> ppExpr expr
        case runInfer expr of
            Left err -> putStrLn $ "Inference error: " <> err
            Right (ty, n, cs) -> do
                putStrLn $ "Inferred type (unresolved): " <> ppType ty
                putStrLn $ "Fresh variable count: " <> show n
                putStrLn $ "Constraints (" <> show (length cs) <> "):"
                mapM_ (putStrLn . ("  " <>) . ppConstraint) cs
                case runSolve cs of
                    Left err -> putStrLn $ "Unification error: " <> err
                    Right subst -> do
                        putStrLn $ "Substitution: " <> ppSubst subst
                        putStrLn $ "Final type: " <> ppType (apply subst ty)
