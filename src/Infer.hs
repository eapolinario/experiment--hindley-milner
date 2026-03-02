{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Algorithm W: Hindley-Milner type inference.
--
-- The main entry point is 'inferExpr', which infers the most general type
-- of a closed expression.
--
-- Internally, 'infer' works in the 'Infer' monad, which provides:
--   * Fresh type-variable generation (via 'StateT Int').
--   * Structured error reporting  (via 'ExceptT InferError').
module Infer
  ( InferError (..)
  , ppError
  , inferExpr
  , inferExprTyped
    -- Exposed for tests / exploration
  , infer
  , inferTyped
  , applyTyped
  , instantiate
  , generalize
  , runInfer
  ) where

import Types
import qualified Subst
import qualified Env
import qualified Unify
import qualified Pretty

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Bifunctor (first)

import Control.Monad.State.Strict
import Control.Monad.Except

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data InferError
  = UnboundVariable String
  | UnificationError Unify.UnifyError
  deriving (Show, Eq)

ppError :: InferError -> String
ppError (UnboundVariable x)   = "Unbound variable: " ++ x
ppError (UnificationError ue) = Unify.ppError ue

-- ---------------------------------------------------------------------------
-- The Infer monad
-- ---------------------------------------------------------------------------

-- | Inference monad: carries a fresh-variable counter and can throw errors.
--
-- @StateT Int@ — integer counter for generating unique variable names.
-- @ExceptT InferError@ — short-circuit on type errors.
newtype Infer a = Infer
  { unInfer :: StateT Int (Either InferError) a }
  deriving ( Functor, Applicative, Monad
           , MonadState Int
           , MonadError InferError )

-- | Run inference, starting the counter at 0.
runInfer :: Infer a -> Either InferError a
runInfer m = evalStateT (unInfer m) 0

-- | Generate a fresh type variable name.
--
-- Counter 0 → "a", 1 → "b", …, 25 → "z", 26 → "a1", 27 → "b1", …
fresh :: Infer TVar
fresh = do
  n <- get
  put (n + 1)
  return (toName n)
  where
    toName n =
      let (q, r) = divMod n 26
          letter  = [toEnum (97 + r)]      -- 'a' .. 'z'
          suffix  = if q == 0 then "" else show q
      in  letter ++ suffix

-- | Lift a unification result into the Infer monad, wrapping the error.
liftUnify :: Either Unify.UnifyError a -> Infer a
liftUnify = liftEither . first UnificationError

-- ---------------------------------------------------------------------------
-- Instantiation and generalization
-- ---------------------------------------------------------------------------

-- | Instantiate a type scheme by replacing every quantified variable with a
-- fresh type variable.
--
-- This is how polymorphism is used: each occurrence of a let-bound name
-- gets its own copy of the quantified variables, so they can be unified
-- independently.
--
-- @∀a b. a → b → a@  becomes  @t₁ → t₂ → t₁@  with fresh @t₁, t₂@.
instantiate :: Scheme -> Infer Ty
instantiate (Scheme vars t) = do
  freshNames <- mapM (const fresh) vars
  let s = Map.fromList (zip vars (map TVar freshNames))
  return (Subst.applyTy s t)

-- | Generalize a type with respect to an environment.
--
-- Produces a scheme by quantifying over all type variables in @t@ that are
-- NOT free in @env@.
--
-- The env constraint is the crucial distinction between let-polymorphism and
-- lambda-polymorphism:
--
--   * Variables free in @env@ are "still being determined" by an outer
--     inference — we must not abstract over them.
--   * Variables free in @t@ but not in @env@ are local to this binding —
--     safe to generalize.
--
-- Called after inferring the right-hand side of a @let@, using the
-- substitution-applied environment @s₁(env)@ so that already-determined
-- variables are correctly excluded.
generalize :: Env.Env -> Ty -> Scheme
generalize env t =
  let envFtvs       = Env.ftv env
      tyFtvs        = Subst.ftvTy t
      generalizable = Set.toList (tyFtvs `Set.difference` envFtvs)
  in  Scheme generalizable t

-- ---------------------------------------------------------------------------
-- Algorithm W
-- ---------------------------------------------------------------------------

-- | Infer the type of expression @e@ in environment @env@.
--
-- Returns @(s, t)@ where:
--   * @s@ is the accumulated substitution (constraints discovered so far).
--   * @t@ is the inferred type of @e@ (may still contain free variables).
--
-- The caller must apply @s@ to @t@ to get the concrete type:
-- @applyTy s t@.
--
-- ===  Substitution threading ===
--
-- This is the #1 source of bugs.  The rule is:
--
--   Before inferring sub-expression @eᵢ@, apply all substitutions discovered
--   so far to @env@ — so that @eᵢ@'s inference sees up-to-date constraints.
--
-- In the @App@ case this manifests as @Env.apply s1 env@ being passed to
-- the second recursive call.
infer :: Env.Env -> Expr -> Infer (Subst, Ty)
infer env = \case

  -- ── Literals ─────────────────────────────────────────────────────────────
  Lit (LInt  _) -> return (Subst.empty, TCon "Int")
  Lit (LBool _) -> return (Subst.empty, TCon "Bool")

  -- ── Variable ─────────────────────────────────────────────────────────────
  -- Look up the scheme, then instantiate it with fresh variables.
  -- No constraints are generated here; unification happens at the call site.
  Var x ->
    case Env.lookupVar x env of
      Nothing     -> throwError (UnboundVariable x)
      Just scheme -> do
        t <- instantiate scheme
        return (Subst.empty, t)

  -- ── Lambda  (λx. e) ──────────────────────────────────────────────────────
  -- Introduce a fresh variable for the argument type.
  -- Lambda-bound variables are monomorphic (Scheme [] argTy) —
  -- they cannot be generalized at this point.
  Lam x body -> do
    argVar <- fresh
    let argTy = TVar argVar
        env'  = Env.extend x (Scheme [] argTy) env
    (s, bodyTy) <- infer env' body
    -- Apply s to argTy: constraints from body inference may have
    -- determined what argTy actually is.
    return (s, TFun (Subst.applyTy s argTy) bodyTy)

  -- ── Application  (f x) ───────────────────────────────────────────────────
  -- The trickiest case.  Substitution must be threaded carefully:
  --
  --   1. Infer f → (s1, funcTy)
  --   2. Infer x in s1(env) → (s2, argTy)
  --      (s1 may have constrained env's variables)
  --   3. Fresh result variable β
  --   4. Unify s2(funcTy) with (argTy → β)
  --      (apply s2 to funcTy because inferring x may have constrained
  --       variables that appear in funcTy)
  --   5. Return (s3 ∘ s2 ∘ s1,  s3(β))
  App func arg -> do
    (s1, funcTy) <- infer env func
    (s2, argTy)  <- infer (Env.apply s1 env) arg
    resVar       <- fresh
    let resTy = TVar resVar
    s3 <- liftUnify $ Unify.unify (Subst.applyTy s2 funcTy) (TFun argTy resTy)
    let s = Subst.compose s3 (Subst.compose s2 s1)
    return (s, Subst.applyTy s3 resTy)

  -- ── Let  (let x = e1 in e2) ──────────────────────────────────────────────
  -- This is where let-polymorphism happens.
  --
  --   1. Infer e1 → (s1, t1)
  --   2. Apply s1 to env (so generalize sees up-to-date constraints).
  --   3. Generalize t1 over s1(env) → scheme σ.
  --      Variables free in s1(env) are not generalized (they're still live).
  --   4. Infer e2 in s1(env) extended with x : σ → (s2, t2).
  --   5. Return (s2 ∘ s1, t2).
  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env'   = Env.apply s1 env
        scheme = generalize env' t1
        env''  = Env.extend x scheme env'
    (s2, t2) <- infer env'' e2
    return (Subst.compose s2 s1, t2)

-- ---------------------------------------------------------------------------
-- Convenient top-level entry point
-- ---------------------------------------------------------------------------

-- | Infer the most general type scheme of a closed expression.
--
-- @
--   inferExpr (Lam "x" (Var "x"))
--     == Right (Scheme ["a"] (TFun (TVar "a") (TVar "a")))
-- @
inferExpr :: Expr -> Either InferError Scheme
inferExpr e = runInfer $ do
  (s, t) <- infer Env.empty e
  let t' = Subst.applyTy s t
  return (Pretty.canonicalize (generalize Env.empty t'))

-- ---------------------------------------------------------------------------
-- Typed AST construction
-- ---------------------------------------------------------------------------

-- | Apply a substitution to every type annotation inside a 'TypedExpr'.
--
-- Called at the end of 'inferExprTyped' to resolve all remaining type
-- variables to their concrete types, producing a fully-annotated AST.
applyTyped :: Subst -> TypedExpr -> TypedExpr
applyTyped s (TELit  l t)        = TELit  l (Subst.applyTy s t)
applyTyped s (TEVar  x t)        = TEVar  x (Subst.applyTy s t)
applyTyped s (TELam  x pt body)  = TELam  x (Subst.applyTy s pt) (applyTyped s body)
applyTyped s (TEApp  f a rt)     = TEApp    (applyTyped s f) (applyTyped s a) (Subst.applyTy s rt)
applyTyped s (TELet  x sc e1 e2) = TELet  x (Subst.applyScheme s sc) (applyTyped s e1) (applyTyped s e2)

-- | Like 'infer', but simultaneously builds the annotated 'TypedExpr'.
--
-- The type annotations stored in the returned 'TypedExpr' may still contain
-- free type variables resolved by the returned substitution.  The caller
-- should apply the substitution with 'applyTyped' to obtain concrete types.
--
-- This mirrors 'infer' case-for-case; the logic is identical, with the
-- addition that each case constructs the corresponding 'TypedExpr' node.
inferTyped :: Env.Env -> Expr -> Infer (Subst, TypedExpr)
inferTyped env = \case

  -- ── Literals ─────────────────────────────────────────────────────────────
  Lit (LInt  n) -> return (Subst.empty, TELit (LInt  n) (TCon "Int"))
  Lit (LBool b) -> return (Subst.empty, TELit (LBool b) (TCon "Bool"))

  -- ── Variable ─────────────────────────────────────────────────────────────
  -- Instantiation gives each use of a let-bound name its own fresh copies
  -- of the quantified variables, recorded in the TEVar annotation.
  Var x ->
    case Env.lookupVar x env of
      Nothing     -> throwError (UnboundVariable x)
      Just scheme -> do
        t <- instantiate scheme
        return (Subst.empty, TEVar x t)

  -- ── Lambda  (λx. e) ──────────────────────────────────────────────────────
  -- Record the (initially unknown) parameter type in the TELam node.
  -- After 'applyTyped' resolves it, the annotation shows the concrete type.
  Lam x body -> do
    argVar <- fresh
    let argTy = TVar argVar
        env'  = Env.extend x (Scheme [] argTy) env
    (s, typedBody) <- inferTyped env' body
    -- argTy may still be a free variable; applyTyped resolves it at the end.
    return (s, TELam x argTy typedBody)

  -- ── Application  (f x) ───────────────────────────────────────────────────
  App func arg -> do
    (s1, typedFunc) <- inferTyped env func
    (s2, typedArg)  <- inferTyped (Env.apply s1 env) arg
    resVar          <- fresh
    let resTy = TVar resVar
    s3 <- liftUnify $
            Unify.unify (Subst.applyTy s2 (typeOf typedFunc))
                        (TFun (typeOf typedArg) resTy)
    let s = Subst.compose s3 (Subst.compose s2 s1)
    -- resTy and the types inside typedFunc/typedArg are resolved by applyTyped.
    return (s, TEApp typedFunc typedArg resTy)

  -- ── Let  (let x = e1 in e2) ──────────────────────────────────────────────
  -- The TELet node records the generalized scheme — the most informative
  -- annotation in the whole typed AST, showing exactly which variables were
  -- quantified and making the generalization step visible.
  Let x e1 e2 -> do
    (s1, typedE1) <- inferTyped env e1
    let env'   = Env.apply s1 env
        -- Apply s1 to get the current best type for generalization.
        t1     = Subst.applyTy s1 (typeOf typedE1)
        scheme = generalize env' t1
        env''  = Env.extend x scheme env'
    (s2, typedE2) <- inferTyped env'' e2
    let s = Subst.compose s2 s1
    return (s, TELet x scheme typedE1 typedE2)

-- | Infer the type of a closed expression and return the fully-annotated
-- 'TypedExpr' — every sub-expression labelled with its inferred type.
--
-- This is the \"typed-AST-out\" entry point of the project:
--
-- @
--   inferExprTyped (Lam "x" (Var "x"))
--     == Right (TELam "x" (TVar "a") (TEVar "x" (TVar "a")))
--   -- typeOf result == TFun (TVar "a") (TVar "a")
--
--   inferExprTyped (App (Lam "x" (Var "x")) (Lit (LInt 1)))
--     == Right (TEApp (TELam "x" (TCon "Int") (TEVar "x" (TCon "Int")))
--                     (TELit (LInt 1) (TCon "Int"))
--                     (TCon "Int"))
-- @
inferExprTyped :: Expr -> Either InferError TypedExpr
inferExprTyped e = runInfer $ do
  (s, te) <- inferTyped Env.empty e
  return (applyTyped s te)
