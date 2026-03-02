{- | Human-readable printing of types, schemes, and expressions.

Call 'ppScheme' on a final inferred type for display.
'canonicalize' renames variables to a, b, c, … in first-occurrence order,
so that @∀b. b -> b@ and @∀a. a -> a@ both display as @∀'a. 'a -> 'a@.
-}
module Pretty (
    ppTy,
    ppScheme,
    ppExpr,
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
