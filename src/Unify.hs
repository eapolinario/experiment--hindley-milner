-- | Unification: given two types, find the most general substitution that
-- makes them equal.
--
-- This is the mathematical heart of Hindley-Milner.  Algorithm W calls
-- 'unify' whenever it discovers a constraint between two types.
module Unify
  ( UnifyError (..)
  , ppError
  , unify
  ) where

import Types
import qualified Subst
import qualified Pretty

import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data UnifyError
  = OccursCheckFailed TVar Ty
    -- ^ @v@ occurs inside @t@: unifying them would produce an infinite type.
    --   Classic example: @unify a (a -> b)@ → fail.
  | TypeMismatch Ty Ty
    -- ^ Two incompatible types with no common unifier.
    --   Example: @unify Int Bool@ → fail.
  deriving (Show, Eq)

ppError :: UnifyError -> String
ppError (OccursCheckFailed v t) =
  "Occurs check failed: '" ++ v ++ " occurs in " ++ Pretty.ppTy t
ppError (TypeMismatch t1 t2) =
  "Cannot unify " ++ Pretty.ppTy t1 ++ " with " ++ Pretty.ppTy t2

-- ---------------------------------------------------------------------------
-- Unification
-- ---------------------------------------------------------------------------

-- | @unify t1 t2@ returns the most general unifier of @t1@ and @t2@, or an
-- error if they cannot be unified.
--
-- Cases:
--
--   * @TVar v@ vs @TVar v@ (same var)  — identity, empty substitution.
--   * @TVar v@ vs any type @t@          — bind @v ↦ t@ (after occurs check).
--   * @TCon c@ vs @TCon c@ (same name)  — trivially equal.
--   * @TFun a b@ vs @TFun a' b'@        — unify args, then unify returns
--                                          under the resulting substitution.
--   * Anything else                      — type mismatch error.
--
-- Substitution threading in the @TFun@ case:
--
-- @
--   s1 ← unify arg1 arg2
--   s2 ← unify (applyTy s1 ret1) (applyTy s1 ret2)   -- apply s1 first!
--   return (compose s2 s1)
-- @
--
-- Applying @s1@ before unifying the return types is essential: @s1@ may
-- have bound a variable that appears in @ret1@ or @ret2@.
unify :: Ty -> Ty -> Either UnifyError Subst
unify (TVar v) t       = bind v t
unify t       (TVar v) = bind v t
unify (TCon c1) (TCon c2)
  | c1 == c2  = Right Subst.empty
  | otherwise = Left (TypeMismatch (TCon c1) (TCon c2))
unify (TFun arg1 ret1) (TFun arg2 ret2) = do
  s1 <- unify arg1 arg2
  s2 <- unify (Subst.applyTy s1 ret1) (Subst.applyTy s1 ret2)
  return (Subst.compose s2 s1)
unify t1 t2 = Left (TypeMismatch t1 t2)

-- ---------------------------------------------------------------------------
-- Variable binding (with occurs check)
-- ---------------------------------------------------------------------------

-- | Attempt to bind type variable @v@ to type @t@.
--
-- Special case: binding @v@ to @TVar v@ (itself) is a no-op.
--
-- Occurs check: if @v@ appears free inside @t@, unifying them would require
-- an infinite type (e.g. @a ~ a -> b@), so we reject it.
bind :: TVar -> Ty -> Either UnifyError Subst
bind v (TVar v') | v == v'                     = Right Subst.empty
bind v t         | Set.member v (Subst.ftvTy t) = Left (OccursCheckFailed v t)
bind v t                                         = Right (Subst.singleton v t)
