"""Unification: given two types, find the most general substitution that
makes them equal.

This is the mathematical heart of Hindley-Milner.  Algorithm W calls
unify() whenever it discovers a constraint between two types.
"""
from __future__ import annotations

from . import pretty as Pretty
from . import subst as Subst
from .types import Subst as SubstT
from .types import TCon, TFun, TVar, Ty

# ---------------------------------------------------------------------------
# Errors
# ---------------------------------------------------------------------------


class OccursCheckFailed(Exception):
    """v occurs inside t: unifying them would produce an infinite type.

    Classic example: unify(TVar("a"), TFun(TVar("a"), TVar("b"))) → fail.
    """

    def __init__(self, var: str, ty: Ty):
        self.var = var
        self.ty = ty
        super().__init__(pp_error(self))


class TypeMismatch(Exception):
    """Two incompatible types with no common unifier.

    Example: unify(TCon("Int"), TCon("Bool")) → fail.
    """

    def __init__(self, t1: Ty, t2: Ty):
        self.t1 = t1
        self.t2 = t2
        super().__init__(pp_error(self))


UnifyError = OccursCheckFailed | TypeMismatch


def pp_error(err: UnifyError) -> str:
    if isinstance(err, OccursCheckFailed):
        return f"Occurs check failed: '{err.var} occurs in {Pretty.pp_ty(err.ty)}"
    elif isinstance(err, TypeMismatch):
        return f"Cannot unify {Pretty.pp_ty(err.t1)} with {Pretty.pp_ty(err.t2)}"
    else:
        raise TypeError(f"Unknown UnifyError: {err!r}")


# ---------------------------------------------------------------------------
# Unification
# ---------------------------------------------------------------------------


def unify(t1: Ty, t2: Ty) -> SubstT:
    """Return the most general unifier of t1 and t2.

    Raises OccursCheckFailed or TypeMismatch on failure.

    Cases:
      * TVar(v) vs TVar(v) (same var)  — identity, empty substitution.
      * TVar(v) vs any type t           — bind v ↦ t (after occurs check).
      * TCon(c) vs TCon(c) (same name)  — trivially equal.
      * TFun(a, b) vs TFun(a', b')      — unify args, then unify returns
                                          under the resulting substitution.
      * Anything else                   — type mismatch error.

    Substitution threading in the TFun case:

        s1 = unify(arg1, arg2)
        s2 = unify(apply_ty(s1, ret1), apply_ty(s1, ret2))   # apply s1 first!
        return compose(s2, s1)

    Applying s1 before unifying the return types is essential: s1 may
    have bound a variable that appears in ret1 or ret2.
    """
    if isinstance(t1, TVar):
        return _bind(t1.name, t2)
    elif isinstance(t2, TVar):
        return _bind(t2.name, t1)
    elif isinstance(t1, TCon) and isinstance(t2, TCon):
        if t1.name == t2.name:
            return Subst.empty()
        raise TypeMismatch(t1, t2)
    elif isinstance(t1, TFun) and isinstance(t2, TFun):
        s1 = unify(t1.arg, t2.arg)
        s2 = unify(Subst.apply_ty(s1, t1.ret), Subst.apply_ty(s1, t2.ret))
        return Subst.compose(s2, s1)
    else:
        raise TypeMismatch(t1, t2)


# ---------------------------------------------------------------------------
# Variable binding (with occurs check)
# ---------------------------------------------------------------------------


def _bind(var: str, ty: Ty) -> SubstT:
    """Attempt to bind type variable var to type ty.

    Special case: binding var to TVar(var) (itself) is a no-op.

    Occurs check: if var appears free inside ty, unifying them would require
    an infinite type (e.g. a ~ a -> b), so we reject it.
    """
    if isinstance(ty, TVar) and ty.name == var:
        return Subst.empty()
    if var in Subst.ftv_ty(ty):
        raise OccursCheckFailed(var, ty)
    return Subst.singleton(var, ty)
