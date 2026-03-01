-- | The type environment: maps variable names to type schemes.
--
-- During inference, the environment records what types have been assigned
-- to the variables that are in scope.  Let-bound variables get polymorphic
-- schemes; lambda-bound variables get monomorphic schemes (@Scheme [] t@).
module Env
  ( Env
  , empty
  , extend
  , lookupVar
  , apply
  , ftv
  ) where

import Types
import qualified Subst

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | The type environment.
type Env = Map String Scheme

-- | The empty environment (no variables in scope).
empty :: Env
empty = Map.empty

-- | Extend the environment with a new variable–scheme binding.
--
-- Shadowing is allowed: @extend x s env@ overwrites any previous binding
-- for @x@, which is correct for nested let / lambda.
extend :: String -> Scheme -> Env -> Env
extend = Map.insert

-- | Look up a variable.  Returns 'Nothing' if it is not in scope.
lookupVar :: String -> Env -> Maybe Scheme
lookupVar = Map.lookup

-- | Apply a substitution to every scheme in the environment.
--
-- Called before inferring sub-expressions so that accumulated constraints
-- are visible while inferring the next piece of the AST.
apply :: Subst -> Env -> Env
apply = Subst.applyEnv

-- | Free type variables in the environment (union over all schemes).
--
-- Used by 'generalize': a type variable that is free in the environment
-- cannot be generalized — it is constrained by the enclosing context.
ftv :: Env -> Set TVar
ftv = Map.foldr (\scheme acc -> acc `Set.union` Subst.ftvScheme scheme) Set.empty
