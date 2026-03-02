"""The type environment: maps variable names to type schemes.

During inference, the environment records what types have been assigned
to the variables that are in scope.  Let-bound variables get polymorphic
schemes; lambda-bound variables get monomorphic schemes (Scheme([], t)).
"""
from __future__ import annotations

from . import subst as Subst
from .types import Scheme

# The type environment is a plain dict: dict[str, Scheme].
Env = dict  # dict[str, Scheme]


def empty() -> Env:
    """The empty environment (no variables in scope)."""
    return {}


def extend(var: str, scheme: Scheme, env: Env) -> Env:
    """Extend the environment with a new variable–scheme binding.

    Shadowing is allowed: overwrites any previous binding for var,
    which is correct for nested let / lambda.

    Returns a new env; does not mutate the original.
    """
    return {**env, var: scheme}


def lookup_var(var: str, env: Env) -> Scheme | None:
    """Look up a variable.  Returns None if it is not in scope."""
    return env.get(var)


def apply(s: dict, env: Env) -> Env:
    """Apply a substitution to every scheme in the environment.

    Called before inferring sub-expressions so that accumulated constraints
    are visible while inferring the next piece of the AST.
    """
    return Subst.apply_env(s, env)


def ftv(env: Env) -> set[str]:
    """Free type variables in the environment (union over all schemes).

    Used by generalize: a type variable that is free in the environment
    cannot be generalized — it is constrained by the enclosing context.
    """
    result: set[str] = set()
    for scheme in env.values():
        result |= Subst.ftv_scheme(scheme)
    return result
