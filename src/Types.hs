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
