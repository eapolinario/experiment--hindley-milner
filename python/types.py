"""Core data types for the Hindley-Milner type inference engine.

Nothing but definitions here — no logic.  Every other module imports this.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Union

# ---------------------------------------------------------------------------
# Type-level primitives
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class TVar:
    """Type variable: still to be determined."""

    name: str


@dataclass(frozen=True)
class TCon:
    """Concrete type constant: 'Int', 'Bool', ..."""

    name: str


@dataclass(frozen=True)
class TFun:
    """Function arrow: arg -> ret"""

    arg: Ty
    ret: Ty


# Sum type for types
Ty = Union[TVar, TCon, TFun]

# ---------------------------------------------------------------------------
# Polymorphism
# ---------------------------------------------------------------------------


@dataclass
class Scheme:
    """A type scheme universally quantifies over a list of type variables.

    Scheme(["a", "b"], TFun(TVar("a"), TVar("b"))) represents ∀a b. a → b.
    Scheme([], t) is a monomorphic type (no quantification).
    """

    vars: list[str]
    ty: Ty


# ---------------------------------------------------------------------------
# Expressions — the input AST
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class LInt:
    """Integer literal."""

    value: int


@dataclass(frozen=True)
class LBool:
    """Boolean literal."""

    value: bool


Lit = Union[LInt, LBool]


@dataclass
class Var:
    """Variable reference."""

    name: str


@dataclass
class App:
    """Function application: func arg"""

    func: Expr
    arg: Expr


@dataclass
class Lam:
    """Lambda abstraction: λvar. body"""

    var: str
    body: Expr


@dataclass
class Let:
    """Let with generalization: let var = e1 in e2"""

    var: str
    e1: Expr
    e2: Expr


@dataclass
class LitExpr:
    """Literal expression wrapper."""

    lit: Lit


Expr = Union[Var, App, Lam, Let, LitExpr]

# ---------------------------------------------------------------------------
# Substitution type
# ---------------------------------------------------------------------------

# A substitution maps type variable names (str) to types (Ty).
# Treated as immutable: always create new dicts rather than mutating.
Subst = dict  # dict[str, Ty]
