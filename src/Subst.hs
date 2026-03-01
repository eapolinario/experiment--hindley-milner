-- | Substitution operations: apply, compose, free type variables.
--
-- A substitution @s :: Subst@ is a finite map from type variables to types.
-- Applying it replaces every free occurrence of a mapped variable.
--
-- The two operations that make Algorithm W tick:
--
--   * 'applyTy'  — walk a type tree, replacing variables.
--   * 'compose'  — chain two substitutions (order matters!).
module Subst
  ( empty
  , singleton
  , applyTy
  , applyScheme
  , applyEnv       -- re-export convenience; implemented here to avoid
                   -- a circular dependency with Env
  , compose
  , ftvTy
  , ftvScheme
  ) where

import Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- Constructors
-- ---------------------------------------------------------------------------

-- | The identity substitution — changes nothing.
empty :: Subst
empty = Map.empty

-- | Bind a single type variable.
singleton :: TVar -> Ty -> Subst
singleton = Map.singleton

-- ---------------------------------------------------------------------------
-- Applying a substitution
-- ---------------------------------------------------------------------------

-- | Apply a substitution to a type.
--
-- Walks the type structure, replacing each type variable @v@ with @s(v)@
-- (or leaving it alone if @v@ is not in the domain of @s@).
--
-- Note: we do NOT recursively apply the substitution to the replacement.
-- That property is guaranteed by 'compose': a well-formed substitution
-- produced by composing unifiers will never need recursive application.
applyTy :: Subst -> Ty -> Ty
applyTy s = \case
  TVar v       -> Map.findWithDefault (TVar v) v s
  TCon c       -> TCon c
  TFun t1 t2   -> TFun (applyTy s t1) (applyTy s t2)

-- | Apply a substitution to a type scheme.
--
-- The quantified variables are bound, so they must be removed from the
-- substitution before applying — otherwise we'd incorrectly substitute them.
applyScheme :: Subst -> Scheme -> Scheme
applyScheme s (Scheme vars t) =
  let s' = foldr Map.delete s vars   -- hide bound vars from s
  in  Scheme vars (applyTy s' t)

-- | Apply a substitution to an entire type environment.
--
-- Defined here (not in Env) to avoid a circular dependency:
--   Subst ← Env  and  Env uses Subst — fine,
--   but Subst using Env would create a cycle.
applyEnv :: Subst -> Map String Scheme -> Map String Scheme
applyEnv s = Map.map (applyScheme s)

-- ---------------------------------------------------------------------------
-- Composition
-- ---------------------------------------------------------------------------

-- | Compose two substitutions.
--
-- @compose s1 s2@ produces the substitution that, when applied, is
-- equivalent to first applying @s2@ and then @s1@:
--
-- @
--   applyTy (compose s1 s2) t  ≡  applyTy s1 (applyTy s2 t)
-- @
--
-- Implementation (Grabmüller formulation):
--
-- @
--   compose s1 s2  =  map (applyTy s1) s2  `Map.union`  s1
-- @
--
-- * The right-hand @s2@ entries get "threaded through" @s1@
--   (so @s2@'s mappings chain into @s1@).
-- * @Map.union@ is left-biased, so the updated @s2@ entries take
--   precedence over @s1@ for variables that appear in both.
--
-- Common mistake: getting the argument order backwards.
-- Think of it as function composition:  @compose s1 s2 = s1 ∘ s2@,
-- with @s2@ applied first.
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (applyTy s1) s2 `Map.union` s1

-- ---------------------------------------------------------------------------
-- Free type variables
-- ---------------------------------------------------------------------------

-- | Free type variables in a type (all of them — types have no binders).
ftvTy :: Ty -> Set TVar
ftvTy = \case
  TVar v     -> Set.singleton v
  TCon _     -> Set.empty
  TFun t1 t2 -> ftvTy t1 `Set.union` ftvTy t2

-- | Free type variables in a scheme.
--
-- The quantified variables are bound, so they are excluded.
ftvScheme :: Scheme -> Set TVar
ftvScheme (Scheme vars t) = ftvTy t `Set.difference` Set.fromList vars
