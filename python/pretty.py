"""Human-readable printing of types, schemes, and expressions.

Call pp_scheme() on a final inferred type for display.
canonicalize() renames variables to a, b, c, … in first-occurrence order,
so that ∀b. b -> b and ∀a. a -> a both display as ∀'a. 'a -> 'a.
"""
from __future__ import annotations

import string
from itertools import count, product

from . import subst as Subst
from .types import (
    App,
    Expr,
    LBool,
    LInt,
    Lam,
    Let,
    LitExpr,
    Scheme,
    TCon,
    TFun,
    TVar,
    Ty,
    Var,
)

# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------


def pp_ty(ty: Ty) -> str:
    """Pretty-print a type.

    Function arrow is right-associative, so we only parenthesize the left
    argument of -> when it is itself a function type:

        'a -> 'b -> 'c          (no parens needed)
        ('a -> 'b) -> 'c        (parens around left TFun)
    """
    if isinstance(ty, TVar):
        return f"'{ty.name}"
    elif isinstance(ty, TCon):
        return ty.name
    elif isinstance(ty, TFun):
        arg_str = f"({pp_ty(ty.arg)})" if isinstance(ty.arg, TFun) else pp_ty(ty.arg)
        return f"{arg_str} -> {pp_ty(ty.ret)}"
    else:
        raise TypeError(f"Unknown Ty node: {ty!r}")


def pp_scheme(scheme: Scheme) -> str:
    """Pretty-print a type scheme.

    A monomorphic scheme (Scheme([], t)) is printed without the quantifier.
    """
    if not scheme.vars:
        return pp_ty(scheme.ty)
    vars_str = " ".join(f"'{v}" for v in scheme.vars)
    return f"∀{vars_str}. {pp_ty(scheme.ty)}"


# ---------------------------------------------------------------------------
# Expressions
# ---------------------------------------------------------------------------


def _pp_lit(lit: object) -> str:
    if isinstance(lit, LInt):
        return str(lit.value)
    elif isinstance(lit, LBool):
        return "true" if lit.value else "false"
    else:
        raise TypeError(f"Unknown Lit: {lit!r}")


def pp_expr(expr: Expr) -> str:
    """Pretty-print an expression.  Application is left-associative;
    the argument is parenthesized when it is itself an application or lambda.
    """
    if isinstance(expr, Var):
        return expr.name
    elif isinstance(expr, LitExpr):
        return _pp_lit(expr.lit)
    elif isinstance(expr, Lam):
        return f"λ{expr.var}. {pp_expr(expr.body)}"
    elif isinstance(expr, Let):
        return f"let {expr.var} = {pp_expr(expr.e1)} in {pp_expr(expr.e2)}"
    elif isinstance(expr, App):
        func_str = f"({pp_expr(expr.func)})" if isinstance(expr.func, Lam) else pp_expr(expr.func)
        arg = expr.arg
        if isinstance(arg, (App, Lam)):
            arg_str = f"({pp_expr(arg)})"
        else:
            arg_str = pp_expr(arg)
        return f"{func_str} {arg_str}"
    else:
        raise TypeError(f"Unknown Expr node: {expr!r}")


# ---------------------------------------------------------------------------
# Canonicalization
# ---------------------------------------------------------------------------


def _canonical_names():
    """Generate canonical variable names: a, b, …, z, a1, b1, …"""
    letters = string.ascii_lowercase
    for c in letters:
        yield c
    for n in count(1):
        for c in letters:
            yield f"{c}{n}"


def _walk_order(ty: Ty) -> list[str]:
    """Walk a type left-to-right, collecting type variable names in order."""
    if isinstance(ty, TVar):
        return [ty.name]
    elif isinstance(ty, TCon):
        return []
    elif isinstance(ty, TFun):
        return _walk_order(ty.arg) + _walk_order(ty.ret)
    else:
        raise TypeError(f"Unknown Ty node: {ty!r}")


def canonicalize(scheme: Scheme) -> Scheme:
    """Rename the quantified variables of a scheme to a, b, c, … in the
    order they first appear (left-to-right) in the type.

    This makes output deterministic regardless of which fresh names were
    generated during inference.

    Example:
        Scheme(["t3"], TFun(TVar("t3"), TVar("t3")))
          → Scheme(["a"], TFun(TVar("a"), TVar("a")))
    """
    bound = set(scheme.vars)
    # Variables in first-occurrence order within the type, filtered to bound vars
    seen: set[str] = set()
    ordered: list[str] = []
    for v in _walk_order(scheme.ty):
        if v in bound and v not in seen:
            ordered.append(v)
            seen.add(v)

    name_gen = _canonical_names()
    new_names = [next(name_gen) for _ in ordered]
    renaming = {old: TVar(new) for old, new in zip(ordered, new_names)}
    new_ty = Subst.apply_ty(renaming, scheme.ty)
    return Scheme(new_names, new_ty)
