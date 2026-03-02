-- | Core data types for the Hindley-Milner type inference engine.
--
-- Nothing but definitions here — no logic.  Every other module imports this.
module Types where

import Data.Map.Strict (Map)

-- ---------------------------------------------------------------------------
-- Type-level primitives
-- ---------------------------------------------------------------------------

-- | A type variable is just a name (e.g. "a", "b", "t1").
type TVar = String

-- | Types — what the inference engine produces.
data Ty
  = TVar TVar      -- ^ Type variable: still to be determined
  | TCon String    -- ^ Concrete type constant: "Int", "Bool", …
  | TFun Ty Ty     -- ^ Function arrow: t₁ → t₂
  deriving (Eq)

-- | Printed as  'a,  Int,  'a -> 'b  (delegates to Pretty in real use,
--   but derived Show is handy for debugging in GHCi).
instance Show Ty where
  show (TVar v)       = "'" ++ v
  show (TCon c)       = c
  show (TFun t1 t2)   = showArg t1 ++ " -> " ++ show t2
    where showArg f@(TFun _ _) = "(" ++ show f ++ ")"
          showArg t             = show t

-- ---------------------------------------------------------------------------
-- Polymorphism
-- ---------------------------------------------------------------------------

-- | A type scheme universally quantifies over a list of type variables.
--
-- @Scheme ["a","b"] (TFun (TVar "a") (TVar "b"))@  represents  @∀a b. a → b@.
--
-- The empty list @Scheme [] t@ is a monomorphic type (no quantification).
data Scheme = Scheme [TVar] Ty
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Expressions — the input AST
-- ---------------------------------------------------------------------------

-- | Integer and boolean literals, for testing convenience.
data Lit
  = LInt  Int
  | LBool Bool
  deriving (Show, Eq)

-- | Expressions.  No parser — build these by hand in tests and the REPL.
--
-- Example:
--
-- @
-- -- λx. x
-- Lam "x" (Var "x")
--
-- -- let id = λx. x in id 1
-- Let "id" (Lam "x" (Var "x")) (App (Var "id") (Lit (LInt 1)))
-- @
data Expr
  = Var String             -- ^ Variable reference
  | App Expr Expr          -- ^ Function application:   f x
  | Lam String Expr        -- ^ Lambda abstraction:     λx. e
  | Let String Expr Expr   -- ^ Let with generalization: let x = e₁ in e₂
  | Lit Lit                -- ^ Literal value
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Substitution type
-- ---------------------------------------------------------------------------

-- | A substitution maps type variables to types.
--
-- It is the fundamental data structure of Algorithm W: unification produces
-- substitutions, and inference threads them through the AST.
type Subst = Map TVar Ty

-- ---------------------------------------------------------------------------
-- Typed (annotated) AST — the "typed-AST-out" of the project description
-- ---------------------------------------------------------------------------

-- | A typed expression mirrors 'Expr' but every node carries its inferred type.
--
-- This is the true output of the type inference engine: not just the top-level
-- type, but a proof that every sub-expression is well-typed.
--
-- Use 'typeOf' to extract the type annotation at any node.
--
-- Construction: call 'Infer.inferExprTyped'.
data TypedExpr
  = TELit  Lit    Ty
    -- ^ Literal with its concrete type.
  | TEVar  String Ty
    -- ^ Variable with its instantiated type (fresh copy of the scheme).
  | TELam  String Ty TypedExpr
    -- ^ @λ(x : paramTy). body@.  The overall type is @TFun paramTy (typeOf body)@.
  | TEApp  TypedExpr TypedExpr Ty
    -- ^ @(func arg) : resTy@.
  | TELet  String Scheme TypedExpr TypedExpr
    -- ^ @let x : σ = e1 in e2@.  The overall type is @typeOf e2@.
  deriving (Show, Eq)

-- | Extract the inferred type of a typed expression.
--
-- For 'TELam' the type is computed as @TFun paramTy (typeOf body)@ rather
-- than stored directly, so the annotation stays in one canonical place.
typeOf :: TypedExpr -> Ty
typeOf (TELit  _ t)          = t
typeOf (TEVar  _ t)          = t
typeOf (TELam  _ paramTy body) = TFun paramTy (typeOf body)
typeOf (TEApp  _ _ resTy)    = resTy
typeOf (TELet  _ _ _ body)   = typeOf body
