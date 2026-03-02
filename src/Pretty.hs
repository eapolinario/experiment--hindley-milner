{- | Human-readable printing of types, schemes, and expressions.

Call 'ppScheme' on a final inferred type for display.
'canonicalize' renames variables to a, b, c, … in first-occurrence order,
so that @∀b. b -> b@ and @∀a. a -> a@ both display as @∀'a. 'a -> 'a@.
-}
module Pretty (
    ppTy,
    ppScheme,
    ppExpr,
    ppTypedExpr,
    canonicalize,
) where

import qualified Subst
import Types

import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

{- | Pretty-print a type.

Function arrow is right-associative, so we only parenthesize the left
argument of @->@ when it is itself a function type:

@
  'a -> 'b -> 'c          (no parens needed)
  ('a -> 'b) -> 'c        (parens around left TFun)
@
-}
ppTy :: Ty -> String
ppTy (TVar v) = "'" ++ v
ppTy (TCon c) = c
ppTy (TFun arg ret) = ppArg arg ++ " -> " ++ ppTy ret
  where
    ppArg f@(TFun _ _) = "(" ++ ppTy f ++ ")"
    ppArg t = ppTy t

{- | Pretty-print a type scheme.

A monomorphic scheme (@Scheme [] t@) is printed without the quantifier.
-}
ppScheme :: Scheme -> String
ppScheme (Scheme [] t) = ppTy t
ppScheme (Scheme vars t) =
    "∀" ++ unwords (map ("'" ++) vars) ++ ". " ++ ppTy t

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

ppLit :: Lit -> String
ppLit (LInt n) = show n
ppLit (LBool b) = if b then "true" else "false"

{- | Pretty-print an expression.  Application is left-associative;
the argument is parenthesized when it is itself an application or lambda.
-}
ppExpr :: Expr -> String
ppExpr (Var x) = x
ppExpr (Lit l) = ppLit l
ppExpr (Lam x body) = "λ" ++ x ++ ". " ++ ppExpr body
ppExpr (Let x e1 e2) =
    "let " ++ x ++ " = " ++ ppExpr e1 ++ " in " ++ ppExpr e2
ppExpr (App f arg) = ppFun f ++ " " ++ ppArg arg
  where
    ppFun e@(Lam _ _) = "(" ++ ppExpr e ++ ")"
    ppFun e = ppExpr e
    ppArg e@(App _ _) = "(" ++ ppExpr e ++ ")"
    ppArg e@(Lam _ _) = "(" ++ ppExpr e ++ ")"
    ppArg e = ppExpr e

-- ---------------------------------------------------------------------------
-- Typed expressions
-- ---------------------------------------------------------------------------

{- | Pretty-print a typed expression, showing type annotations at every non-literal node.

Format:

  * Literals:     @42@  (type is obvious from value; annotation omitted)
  * Variables:    @x : T@
  * Lambdas:      @λ(x : T₁). body@   (whole type = @T₁ -> typeOf body@)
  * Application:  @(f) arg : T@
  * Let:          @let x : σ = e1 in e2@

The function in an application is always wrapped in @(…)@ to prevent the
function's @: T@ annotation from being read as part of the argument list.

Example — @let id = λx. x in id 1@:

@
  let id : ∀'a. 'a -> 'a = λ(x : 'a). x : 'a in (id : Int -> Int) 1 : Int
@

The lambda's body shows @x : 'a@ (still polymorphic at the definition site).
The application site shows @id : Int -> Int@ (instantiated for this call).
This makes instantiation and generalization visually obvious.
-}
ppTypedExpr :: TypedExpr -> String
ppTypedExpr = goTop
  where
    -- Literals: no annotation — type is self-evident.
    goTop (TELit l _) =
        ppLit l
    -- Variables: always show the instantiated type.
    goTop (TEVar x t) =
        x ++ " : " ++ ppTy t
    -- Lambdas: show parameter type; body is printed recursively.
    goTop (TELam x pt body) =
        "λ(" ++ x ++ " : " ++ ppTy pt ++ "). " ++ goTop body
    -- Applications: wrap the function in parens to avoid annotation ambiguity,
    -- then show the result type at the end.
    goTop (TEApp f a resTy) =
        goFun f ++ " " ++ goArg a ++ " : " ++ ppTy resTy
    -- Let: show the generalized scheme of the bound variable.
    goTop (TELet x sc e1 e2) =
        "let " ++ x ++ " : " ++ ppScheme sc
        ++ " = " ++ goTop e1
        ++ " in " ++ goTop e2

    -- Function position: always wrap in parens.
    -- This prevents "x : Int -> Int arg : T" being misread as a type.
    goFun e = "(" ++ goTop e ++ ")"

    -- Argument position: wrap only compound forms that need disambiguation.
    goArg e@(TEApp _ _ _)   = "(" ++ goTop e ++ ")"
    goArg e@(TELam _ _ _)   = "(" ++ goTop e ++ ")"
    goArg e@(TELet _ _ _ _) = "(" ++ goTop e ++ ")"
    goArg e                 = goTop e

-- ---------------------------------------------------------------------------
-- Canonicalization
-- ---------------------------------------------------------------------------

{- | Rename the quantified variables of a scheme to @a, b, c, …@ in the
order they first appear (left-to-right) in the type.

This makes output deterministic regardless of which fresh names were
generated during inference.

Examples:

@
  Scheme ["t3"] (TFun (TVar "t3") (TVar "t3"))
    → Scheme ["a"] (TFun (TVar "a") (TVar "a"))
@
-}
canonicalize :: Scheme -> Scheme
canonicalize (Scheme vars t) =
    let
        -- Variables in first-occurrence order within the type
        ordered = nub [v | v <- walkOrder t, v `elem` vars]
        -- Canonical names: a, b, …, z, a1, b1, …
        names = [[c] | c <- ['a' .. 'z']] ++ [c : show n | n <- [1 :: Int ..], c <- ['a' .. 'z']]
        renaming = Map.fromList (zip ordered (map TVar names))
        newVars = take (length ordered) names
     in
        Scheme newVars (Subst.applyTy renaming t)

-- | Walk a type left-to-right, collecting type variable names in order.
walkOrder :: Ty -> [TVar]
walkOrder (TVar v) = [v]
walkOrder (TCon _) = []
walkOrder (TFun t1 t2) = walkOrder t1 ++ walkOrder t2
