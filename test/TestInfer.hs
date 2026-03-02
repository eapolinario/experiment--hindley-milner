{- | Test suite for Algorithm W.

Each test builds an expression by hand, runs inference, and checks the
result against an expected type string.

Run with:  cabal test
-}
module Main where

import Test.Hspec (Expectation, describe, expectationFailure, hspec, it, shouldBe)

import Infer (inferExpr, ppError)
import Pretty (ppScheme)
import Types

-- ---------------------------------------------------------------------------
-- Assertions
-- ---------------------------------------------------------------------------

shouldInfer :: Expr -> String -> Expectation
shouldInfer expr expected =
    case inferExpr expr of
        Left err ->
            if expected == "error"
                then pure ()
                else
                    expectationFailure $
                        unlines
                            [ "expected: " ++ expected
                            , "got error: " ++ ppError err
                            ]
        Right scheme ->
            let got = ppScheme scheme
             in if expected == "error"
                    then expectationFailure $ "expected error, but got: " ++ got
                    else got `shouldBe` expected

-- ---------------------------------------------------------------------------
-- Helper constructors (make AST-building less noisy)
-- ---------------------------------------------------------------------------

-- | @lam "x" e@  ≡  Lam "x" e
lam :: String -> Expr -> Expr
lam = Lam

-- | @v "x"@  ≡  Var "x"
v :: String -> Expr
v = Var

-- | @app f x@  ≡  App f x  (left-associative)
app :: Expr -> Expr -> Expr
app = App

-- | @lett "x" e1 e2@  ≡  Let "x" e1 e2
lett :: String -> Expr -> Expr -> Expr
lett = Let

int :: Int -> Expr
int = Lit . LInt

bool :: Bool -> Expr
bool = Lit . LBool

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "Phase 1 & 2: Literals" $ do
        -- Literals infer to their base types immediately.
        it "int literal" $
            shouldInfer (int 42) "Int"

        it "bool literal" $
            shouldInfer (bool True) "Bool"

    describe "Phase 3: Lambda / application / let" $ do
        -- λx. x  :  ∀'a. 'a -> 'a
        -- The classic identity function.
        it "identity" $
            shouldInfer (lam "x" (v "x")) "∀'a. 'a -> 'a"

        -- (λx. x) 42  :  Int
        -- Applying identity to an Int specializes the type.
        it "identity applied to int" $
            shouldInfer (app (lam "x" (v "x")) (int 1)) "Int"

        -- λx. λy. x  :  ∀'a 'b. 'a -> 'b -> 'a
        -- The const combinator.
        it "const" $
            shouldInfer (lam "x" (lam "y" (v "x"))) "∀'a 'b. 'a -> 'b -> 'a"

        -- λf. f 1  :  ∀'a. (Int -> 'a) -> 'a
        -- Applying an unknown function to an Int constrains its argument type.
        it "apply function to int" $
            shouldInfer (lam "f" (app (v "f") (int 1))) "∀'a. (Int -> 'a) -> 'a"

        -- let id = λx. x in id 1  :  Int
        -- let-polymorphism: id is generalized, then used at Int.
        it "let id, apply to int" $
            shouldInfer (lett "id" (lam "x" (v "x")) (app (v "id") (int 1))) "Int"

        -- let id = λx. x in id  :  ∀'a. 'a -> 'a
        -- The identity scheme is preserved end-to-end.
        it "let id, return id" $
            shouldInfer (lett "id" (lam "x" (v "x")) (v "id")) "∀'a. 'a -> 'a"

        -- let f = λx. x in let g = f in g  :  ∀'a. 'a -> 'a
        -- Polymorphism flows through a second let binding.
        it "let chain: let f = id in let g = f in g" $
            shouldInfer
                ( lett
                    "f"
                    (lam "x" (v "x"))
                    (lett "g" (v "f") (v "g"))
                )
                "∀'a. 'a -> 'a"

    describe "Phase 3: Polymorphism milestones" $ do
        -- let id = λx. x in id applied to two different types (sequenced).
        -- We encode "(id 1, id true)" as "id (id 1)" since we have no tuples yet;
        -- the important test is that using id at Int does not prevent using it at Bool.
        it "id used at Int then Bool (sequenced)" $
            shouldInfer
                ( lett
                    "id"
                    (lam "x" (v "x"))
                    ( lett
                        "a"
                        (app (v "id") (int 1))
                        (app (v "id") (bool True))
                    )
                )
                "Bool"

    describe "Classic combinators" $ do
        -- λf. λg. λx. f (g x)  :  ∀'a 'b 'c. ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
        -- Canonical form: f's type appears first in the tree, so f : 'a -> 'b,
        -- then g's output must match f's input: g : 'c -> 'a.
        it "compose" $
            shouldInfer
                (lam "f" (lam "g" (lam "x" (app (v "f") (app (v "g") (v "x"))))))
                "∀'a 'b 'c. ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b"

        -- λf. λx. f (f x)  :  ∀'a. ('a -> 'a) -> 'a -> 'a
        -- f must have the same domain and codomain for f (f x) to typecheck.
        it "apply-twice" $
            shouldInfer
                (lam "f" (lam "x" (app (v "f") (app (v "f") (v "x")))))
                "∀'a. ('a -> 'a) -> 'a -> 'a"

        -- λx. λy. y  :  ∀'a 'b. 'a -> 'b -> 'b
        it "second projection" $
            shouldInfer
                (lam "x" (lam "y" (v "y")))
                "∀'a 'b. 'a -> 'b -> 'b"

        -- λf. λg. λx. f x (g x)  :  ∀'a 'b 'c. ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
        -- The S combinator. Exercises three simultaneous constraints in App.
        it "S combinator" $
            shouldInfer
                (lam "f" (lam "g" (lam "x" (app (app (v "f") (v "x")) (app (v "g") (v "x"))))))
                "∀'a 'b 'c. ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c"

        -- λx. λy. x y  :  ∀'a 'b. ('a -> 'b) -> 'a -> 'b
        it "apply (flip of $)" $
            shouldInfer
                (lam "x" (lam "y" (app (v "x") (v "y"))))
                "∀'a 'b. ('a -> 'b) -> 'a -> 'b"

        -- (λx. x) (λx. x)  :  ∀'a. 'a -> 'a
        -- Identity applied to itself — tests instantiation at a function type.
        it "identity applied to itself" $
            shouldInfer
                (app (lam "x" (v "x")) (lam "x" (v "x")))
                "∀'a. 'a -> 'a"

    describe "Let-polymorphism stress tests" $ do
        -- let id = λx. x in id id  :  ∀'a. 'a -> 'a
        -- id is instantiated twice: once as ('a -> 'a) -> ('a -> 'a), once as 'a -> 'a.
        it "id applied to id" $
            shouldInfer
                (lett "id" (lam "x" (v "x")) (app (v "id") (v "id")))
                "∀'a. 'a -> 'a"

        -- let id = λx. x in id id 1  :  Int
        -- id instantiated twice in one expression, then applied to Int.
        it "id id 1" $
            shouldInfer
                (lett "id" (lam "x" (v "x")) (app (app (v "id") (v "id")) (int 1)))
                "Int"

        -- let k = λx. λy. x in let k' = k in k'  :  ∀'a 'b. 'a -> 'b -> 'a
        -- Polymorphism of a two-argument function survives a second let binding.
        it "const through two lets" $
            shouldInfer
                (lett "k" (lam "x" (lam "y" (v "x"))) (lett "k'" (v "k") (v "k'")))
                "∀'a 'b. 'a -> 'b -> 'a"

    describe "Shadowing" $ do
        -- let x = 1 in let x = true in x  :  Bool
        it "inner let shadows outer" $
            shouldInfer
                (lett "x" (int 1) (lett "x" (bool True) (v "x")))
                "Bool"

    describe "Expected failures" $ do
        -- λx. x x  →  occurs-check failure (x : a, x x requires a ~ a -> b)
        it "self-application (occurs check)" $
            shouldInfer (lam "x" (app (v "x") (v "x"))) "error"

        -- λx. x x x  →  same root cause, one level deeper
        it "self-application chained" $
            shouldInfer (lam "x" (app (app (v "x") (v "x")) (v "x"))) "error"

        -- Applying a non-function: 1 true  →  Int is not a function type
        it "apply non-function" $
            shouldInfer (app (int 1) (bool True)) "error"

        -- Unbound variable
        it "unbound variable" $
            shouldInfer (v "z") "error"

        -- λf. (f 1, f true) would fail in HM because f is lambda-bound (monomorphic).
        -- We encode it as:  λf. let _ = f 1 in f true
        -- This should fail because unifying f : Int -> b with f : Bool -> c
        -- forces Int ~ Bool.
        it "lambda-bound f used at two types (should fail)" $
            shouldInfer
                ( lam
                    "f"
                    ( lett
                        "_"
                        (app (v "f") (int 1))
                        (app (v "f") (bool True))
                    )
                )
                "error"
