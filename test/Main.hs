module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec

import Sina


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
    testGroup
        "Parser"
        [ testGroup
            "Literals"
            [ testCase "integer" $
                parse parseExpr "test" "42" @?= Right (EInt 42)
            , testCase "True" $
                parse parseExpr "test" "True" @?= Right (EBool True)
            , testCase "False" $
                parse parseExpr "test" "False" @?= Right (EBool False)
            , testCase "variable" $
                parse parseExpr "test" "x" @?= Right (EVar "x")
            , testCase "multi-char variable" $
                parse parseExpr "test" "foo" @?= Right (EVar "foo")
            ]
        , testGroup
            "Parentheses"
            [ testCase "parenthesized expression" $
                parse parseExpr "test" "(42)" @?= Right (EInt 42)
            , testCase "parenthesized variable" $
                parse parseExpr "test" "(x)" @?= Right (EVar "x")
            ]
        , testGroup
            "Application"
            [ testCase "simple application" $
                parse parseExpr "test" "f x" @?= Right (EApp (EVar "f") (EVar "x"))
            , testCase "multi-argument application" $
                parse parseExpr "test" "f x y"
                    @?= Right (EApp (EApp (EVar "f") (EVar "x")) (EVar "y"))
            , testCase "application with integer" $
                parse parseExpr "test" "x 42" @?= Right (EApp (EVar "x") (EInt 42))
            ]
        , testGroup
            "Abstraction"
            [ testCase "simple abstraction" $
                parse parseExpr "test" "\\x -> x"
                    @?= Right (EAbs "x" (EVar "x"))
            , testCase "abstraction with body application" $
                parse parseExpr "test" "\\f -> f x"
                    @?= Right (EAbs "f" (EApp (EVar "f") (EVar "x")))
            ]
        , testGroup
            "Let"
            [ testCase "simple let" $
                parse parseExpr "test" "let x = 42 in x"
                    @?= Right (ELet "x" (EInt 42) (EVar "x"))
            , testCase "let with abstraction" $
                parse parseExpr "test" "let id = \\x -> x in id 42"
                    @?= Right
                        ( ELet
                            "id"
                            (EAbs "x" (EVar "x"))
                            (EApp (EVar "id") (EInt 42))
                        )
            ]
        , testGroup
            "If"
            [ testCase "simple if" $
                parse parseExpr "test" "if True then 1 else 0"
                    @?= Right (EIf (EBool True) (EInt 1) (EInt 0))
            , testCase "if with nested expression" $
                parse parseExpr "test" "if x then y else z"
                    @?= Right (EIf (EVar "x") (EVar "y") (EVar "z"))
            ]
        , testGroup
            "Binary operators"
            [ testCase "addition" $
                parse parseExpr "test" "1 + 2"
                    @?= Right (EBin Add (EInt 1) (EInt 2))
            , testCase "subtraction" $
                parse parseExpr "test" "3 - 1"
                    @?= Right (EBin Sub (EInt 3) (EInt 1))
            , testCase "addition is left-associative" $
                parse parseExpr "test" "1 + 2 + 3"
                    @?= Right
                        (EBin Add (EBin Add (EInt 1) (EInt 2)) (EInt 3))
            ]
        , testGroup
            "Reserved words"
            [ testCase "if fails as identifier" $
                case parse parseExpr "test" "if" of
                    Left _ -> pure ()
                    Right _ -> assertFailure "expected parse failure"
            , testCase "True fails as identifier" $
                case parse parseExpr "test" "True" of
                    Right (EVar "True") -> assertFailure "expected parse failure"
                    _ -> pure ()
            , testCase "let fails as identifier" $
                case parse parseExpr "test" "let" of
                    Left _ -> pure ()
                    Right _ -> assertFailure "expected parse failure"
            ]
        , testGroup
            "Round-trip pretty-print"
            [ testCase "integer round-trip" $
                ppExpr (EInt 42) @?= "42"
            , testCase "abstraction round-trip" $
                ppExpr (EAbs "x" (EVar "x")) @?= "\\x -> x"
            , testCase "let round-trip" $
                ppExpr (ELet "x" (EInt 42) (EVar "x")) @?= "let x = 42 in x"
            ]
        ]
