"""Substitution operations: apply, compose, free type variables.

A substitution s :: Subst is a finite map from type variables to types.
Applying it replaces every free occurrence of a mapped variable.

The two operations that make Algorithm W tick:
  * apply_ty  — walk a type tree, replacing variables.
  * compose   — chain two substitutions (order matters!).
"""
from __future__ import annotations

from .types import Scheme, Subst, TCon, TFun, TVar, Ty

# ---------------------------------------------------------------------------
# Constructors
# ---------------------------------------------------------------------------


def empty() -> Subst:
    """The identity substitution — changes nothing."""
    return {}


def singleton(var: str, ty: Ty) -> Subst:
    """Bind a single type variable."""
    return {var: ty}


# ---------------------------------------------------------------------------
# Applying a substitution
# ---------------------------------------------------------------------------


def apply_ty(s: Subst, ty: Ty) -> Ty:
    """Apply a substitution to a type.

    Walks the type structure, replacing each type variable v with s(v)
    (or leaving it alone if v is not in the domain of s).

    Note: we do NOT recursively apply the substitution to the replacement.
    That property is guaranteed by compose: a well-formed substitution
    produced by composing unifiers will never need recursive application.
    """
    if isinstance(ty, TVar):
        return s.get(ty.name, ty)
    elif isinstance(ty, TCon):
        return ty
    elif isinstance(ty, TFun):
        return TFun(apply_ty(s, ty.arg), apply_ty(s, ty.ret))
    else:
        raise TypeError(f"Unknown Ty node: {ty!r}")


def apply_scheme(s: Subst, scheme: Scheme) -> Scheme:
    """Apply a substitution to a type scheme.

    The quantified variables are bound, so they must be removed from the
    substitution before applying — otherwise we'd incorrectly substitute them.
    """
    s_restricted = {k: v for k, v in s.items() if k not in scheme.vars}
    return Scheme(scheme.vars, apply_ty(s_restricted, scheme.ty))


def apply_env(s: Subst, env: dict) -> dict:
    """Apply a substitution to an entire type environment."""
    return {k: apply_scheme(s, v) for k, v in env.items()}


# ---------------------------------------------------------------------------
# Composition
# ---------------------------------------------------------------------------


def compose(s1: Subst, s2: Subst) -> Subst:
    """Compose two substitutions.

    compose(s1, s2) produces the substitution that, when applied, is
    equivalent to first applying s2 and then s1:

        apply_ty(compose(s1, s2), t)  ==  apply_ty(s1, apply_ty(s2, t))

    Implementation (Grabmüller formulation):

        compose(s1, s2) = {k: apply_ty(s1, v) for k, v in s2.items()} | s1

    * The s2 entries get "threaded through" s1 (so s2's mappings chain into s1).
    * The | merge is left-biased, so the updated s2 entries take precedence
      over s1 for variables that appear in both.

    Common mistake: getting the argument order backwards.
    Think of it as function composition: compose(s1, s2) = s1 ∘ s2,
    with s2 applied first.
    """
    updated_s2 = {k: apply_ty(s1, v) for k, v in s2.items()}
    return updated_s2 | s1


# ---------------------------------------------------------------------------
# Free type variables
# ---------------------------------------------------------------------------


def ftv_ty(ty: Ty) -> set[str]:
    """Free type variables in a type (all of them — types have no binders)."""
    if isinstance(ty, TVar):
        return {ty.name}
    elif isinstance(ty, TCon):
        return set()
    elif isinstance(ty, TFun):
        return ftv_ty(ty.arg) | ftv_ty(ty.ret)
    else:
        raise TypeError(f"Unknown Ty node: {ty!r}")


def ftv_scheme(scheme: Scheme) -> set[str]:
    """Free type variables in a scheme.

    The quantified variables are bound, so they are excluded.
    """
    return ftv_ty(scheme.ty) - set(scheme.vars)
