-- | Test suite for Algorithm W.
--
-- Each test builds an expression by hand, runs inference, and checks the
-- result against an expected type string.
--
-- Run with:  cabal test
module Main where

import Types
import Infer  (inferExpr, ppError)
import Pretty (ppScheme)

-- ---------------------------------------------------------------------------
-- Tiny test runner (no external deps required)
-- ---------------------------------------------------------------------------

data Result = Pass | Fail String deriving (Eq)

check :: String -> Expr -> String -> IO ()
check name expr expected =
  case inferExpr expr of
    Left err ->
      if expected == "error"
        then putStrLn $ "PASS  " ++ name
        else do
          putStrLn $ "FAIL  " ++ name
          putStrLn $ "      expected : " ++ expected
          putStrLn $ "      got error: " ++ ppError err
    Right scheme ->
      let got = ppScheme scheme
      in if got == expected
           then putStrLn $ "PASS  " ++ name ++ "  :  " ++ got
           else do
             putStrLn $ "FAIL  " ++ name
             putStrLn $ "      expected: " ++ expected
             putStrLn $ "      got:      " ++ got

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
int  = Lit . LInt

bool :: Bool -> Expr
bool = Lit . LBool

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Phase 1 & 2: Literals ==="

  -- Literals infer to their base types immediately.
  check "int literal"   (int 42)       "Int"
  check "bool literal"  (bool True)    "Bool"

  putStrLn ""
  putStrLn "=== Phase 3: Lambda / application / let ==="

  -- λx. x  :  ∀'a. 'a -> 'a
  -- The classic identity function.
  check "identity"
    (lam "x" (v "x"))
    "∀'a. 'a -> 'a"

  -- (λx. x) 42  :  Int
  -- Applying identity to an Int specializes the type.
  check "identity applied to int"
    (app (lam "x" (v "x")) (int 1))
    "Int"

  -- λx. λy. x  :  ∀'a 'b. 'a -> 'b -> 'a
  -- The const combinator.
  check "const"
    (lam "x" (lam "y" (v "x")))
    "∀'a 'b. 'a -> 'b -> 'a"

  -- λf. f 1  :  ∀'a. (Int -> 'a) -> 'a
  -- Applying an unknown function to an Int constrains its argument type.
  check "apply function to int"
    (lam "f" (app (v "f") (int 1)))
    "∀'a. (Int -> 'a) -> 'a"

  -- let id = λx. x in id 1  :  Int
  -- let-polymorphism: id is generalized, then used at Int.
  check "let id, apply to int"
    (lett "id" (lam "x" (v "x")) (app (v "id") (int 1)))
    "Int"

  -- let id = λx. x in id  :  ∀'a. 'a -> 'a
  -- The identity scheme is preserved end-to-end.
  check "let id, return id"
    (lett "id" (lam "x" (v "x")) (v "id"))
    "∀'a. 'a -> 'a"

  -- let f = λx. x in let g = f in g  :  ∀'a. 'a -> 'a
  -- Polymorphism flows through a second let binding.
  check "let chain: let f = id in let g = f in g"
    (lett "f" (lam "x" (v "x"))
      (lett "g" (v "f") (v "g")))
    "∀'a. 'a -> 'a"

  putStrLn ""
  putStrLn "=== Phase 3: Polymorphism milestones ==="

  -- let id = λx. x in id applied to two different types (sequenced).
  -- We encode "(id 1, id true)" as "id (id 1)" since we have no tuples yet;
  -- the important test is that using id at Int does not prevent using it at Bool.
  check "id used at Int then Bool (sequenced)"
    (lett "id" (lam "x" (v "x"))
      (lett "a" (app (v "id") (int 1))
        (app (v "id") (bool True))))
    "Bool"

  putStrLn ""
  putStrLn "=== Phase 3: Expected failures ==="

  -- λx. x x  →  occurs-check failure (x : a, x x requires a ~ a -> b)
  check "self-application (occurs check)"
    (lam "x" (app (v "x") (v "x")))
    "error"

  -- Unbound variable
  check "unbound variable"
    (v "z")
    "error"

  -- λf. (f 1, f true) would fail in HM because f is lambda-bound (monomorphic).
  -- We encode it as:  λf. let _ = f 1 in f true
  -- This should fail because unifying f : Int -> b with f : Bool -> c
  -- forces Int ~ Bool.
  check "lambda-bound f used at two types (should fail)"
    (lam "f"
      (lett "_" (app (v "f") (int 1))
        (app (v "f") (bool True))))
    "error"

  putStrLn ""
  putStrLn "Done."
