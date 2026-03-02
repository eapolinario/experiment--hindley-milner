"""Algorithm W: Hindley-Milner type inference.

The main entry point is infer_expr(), which infers the most general type
of a closed expression.

Internally, inference is carried out by an InferEngine instance, which
maintains a fresh type-variable counter and raises InferError on failure.
"""
from __future__ import annotations

from . import env as Env
from . import pretty as Pretty
from . import subst as Subst
from . import unify as Unify
from .types import (
    App,
    Expr,
    LBool,
    LInt,
    Lam,
    Let,
    LitExpr,
    Scheme,
    Subst as SubstT,
    TCon,
    TFun,
    TVar,
    Ty,
    Var,
)

# ---------------------------------------------------------------------------
# Errors
# ---------------------------------------------------------------------------


class UnboundVariable(Exception):
    def __init__(self, name: str):
        self.name = name
        super().__init__(f"Unbound variable: {name}")


class UnificationError(Exception):
    def __init__(self, cause: Unify.UnifyError):
        self.cause = cause
        super().__init__(str(cause))


InferError = UnboundVariable | UnificationError


def pp_error(err: Exception) -> str:
    if isinstance(err, UnboundVariable):
        return f"Unbound variable: {err.name}"
    elif isinstance(err, UnificationError):
        return Unify.pp_error(err.cause)
    else:
        return str(err)


# ---------------------------------------------------------------------------
# Name generation
# ---------------------------------------------------------------------------

_LETTERS = "abcdefghijklmnopqrstuvwxyz"


def _to_name(n: int) -> str:
    """Counter 0 → 'a', 1 → 'b', …, 25 → 'z', 26 → 'a1', 27 → 'b1', …"""
    q, r = divmod(n, 26)
    letter = _LETTERS[r]
    suffix = "" if q == 0 else str(q)
    return letter + suffix


# ---------------------------------------------------------------------------
# Inference engine
# ---------------------------------------------------------------------------


class InferEngine:
    """Carries the fresh type-variable counter for a single inference run."""

    def __init__(self) -> None:
        self._counter = 0

    def fresh(self) -> str:
        """Generate a fresh type variable name."""
        name = _to_name(self._counter)
        self._counter += 1
        return name

    # -------------------------------------------------------------------------
    # Instantiation and generalization
    # -------------------------------------------------------------------------

    def instantiate(self, scheme: Scheme) -> Ty:
        """Instantiate a type scheme by replacing every quantified variable
        with a fresh type variable.

        This is how polymorphism is used: each occurrence of a let-bound name
        gets its own copy of the quantified variables, so they can be unified
        independently.

        ∀a b. a → b → a  becomes  t₁ → t₂ → t₁  with fresh t₁, t₂.
        """
        fresh_names = [self.fresh() for _ in scheme.vars]
        renaming = {old: TVar(new) for old, new in zip(scheme.vars, fresh_names)}
        return Subst.apply_ty(renaming, scheme.ty)

    def generalize(self, env: Env.Env, ty: Ty) -> Scheme:
        """Generalize a type with respect to an environment.

        Produces a scheme by quantifying over all type variables in ty that
        are NOT free in env.

        The env constraint is the crucial distinction between let-polymorphism
        and lambda-polymorphism:
          * Variables free in env are "still being determined" by an outer
            inference — we must not abstract over them.
          * Variables free in ty but not in env are local to this binding —
            safe to generalize.

        Called after inferring the RHS of a let, using the substitution-applied
        environment s1(env) so that already-determined variables are excluded.
        """
        env_ftvs = Env.ftv(env)
        ty_ftvs = Subst.ftv_ty(ty)
        generalizable = sorted(ty_ftvs - env_ftvs)
        return Scheme(generalizable, ty)

    # -------------------------------------------------------------------------
    # Algorithm W
    # -------------------------------------------------------------------------

    def infer(self, env: Env.Env, expr: Expr) -> tuple[SubstT, Ty]:
        """Infer the type of expression expr in environment env.

        Returns (s, t) where:
          * s is the accumulated substitution (constraints discovered so far).
          * t is the inferred type of expr (may still contain free variables).

        The caller must apply s to t to get the concrete type: apply_ty(s, t).

        === Substitution threading ===

        This is the #1 source of bugs.  The rule is:

          Before inferring sub-expression eᵢ, apply all substitutions
          discovered so far to env — so that eᵢ's inference sees up-to-date
          constraints.

        In the App case this manifests as Env.apply(s1, env) being passed to
        the second recursive call.
        """
        # ── Literals ──────────────────────────────────────────────────────────
        if isinstance(expr, LitExpr):
            if isinstance(expr.lit, LInt):
                return Subst.empty(), TCon("Int")
            elif isinstance(expr.lit, LBool):
                return Subst.empty(), TCon("Bool")

        # ── Variable ──────────────────────────────────────────────────────────
        # Look up the scheme, then instantiate it with fresh variables.
        # No constraints are generated here; unification happens at the call site.
        elif isinstance(expr, Var):
            scheme = Env.lookup_var(expr.name, env)
            if scheme is None:
                raise UnboundVariable(expr.name)
            ty = self.instantiate(scheme)
            return Subst.empty(), ty

        # ── Lambda  (λx. e) ───────────────────────────────────────────────────
        # Introduce a fresh variable for the argument type.
        # Lambda-bound variables are monomorphic (Scheme([], argTy)) —
        # they cannot be generalized at this point.
        elif isinstance(expr, Lam):
            arg_var = self.fresh()
            arg_ty = TVar(arg_var)
            env2 = Env.extend(expr.var, Scheme([], arg_ty), env)
            s, body_ty = self.infer(env2, expr.body)
            # Apply s to arg_ty: constraints from body inference may have
            # determined what arg_ty actually is.
            return s, TFun(Subst.apply_ty(s, arg_ty), body_ty)

        # ── Application  (f x) ────────────────────────────────────────────────
        # The trickiest case.  Substitution must be threaded carefully:
        #
        #   1. Infer f → (s1, func_ty)
        #   2. Infer x in s1(env) → (s2, arg_ty)
        #      (s1 may have constrained env's variables)
        #   3. Fresh result variable β
        #   4. Unify s2(func_ty) with (arg_ty → β)
        #      (apply s2 to func_ty because inferring x may have constrained
        #       variables that appear in func_ty)
        #   5. Return (s3 ∘ s2 ∘ s1,  s3(β))
        elif isinstance(expr, App):
            s1, func_ty = self.infer(env, expr.func)
            s2, arg_ty = self.infer(Env.apply(s1, env), expr.arg)
            res_var = self.fresh()
            res_ty = TVar(res_var)
            try:
                s3 = Unify.unify(Subst.apply_ty(s2, func_ty), TFun(arg_ty, res_ty))
            except (Unify.OccursCheckFailed, Unify.TypeMismatch) as e:
                raise UnificationError(e) from e
            s = Subst.compose(s3, Subst.compose(s2, s1))
            return s, Subst.apply_ty(s3, res_ty)

        # ── Let  (let x = e1 in e2) ───────────────────────────────────────────
        # This is where let-polymorphism happens.
        #
        #   1. Infer e1 → (s1, t1)
        #   2. Apply s1 to env (so generalize sees up-to-date constraints).
        #   3. Generalize t1 over s1(env) → scheme σ.
        #      Variables free in s1(env) are not generalized (still live).
        #   4. Infer e2 in s1(env) extended with x : σ → (s2, t2).
        #   5. Return (s2 ∘ s1, t2).
        elif isinstance(expr, Let):
            s1, t1 = self.infer(env, expr.e1)
            env2 = Env.apply(s1, env)
            scheme = self.generalize(env2, t1)
            env3 = Env.extend(expr.var, scheme, env2)
            s2, t2 = self.infer(env3, expr.e2)
            return Subst.compose(s2, s1), t2

        else:
            raise TypeError(f"Unknown Expr node: {expr!r}")


# ---------------------------------------------------------------------------
# Convenient top-level entry point
# ---------------------------------------------------------------------------


def infer_expr(expr: Expr) -> Scheme:
    """Infer the most general type scheme of a closed expression.

    Raises UnboundVariable or UnificationError on failure.

    Example:
        infer_expr(Lam("x", Var("x")))
          == Scheme(["a"], TFun(TVar("a"), TVar("a")))
    """
    engine = InferEngine()
    s, t = engine.infer(Env.empty(), expr)
    t_final = Subst.apply_ty(s, t)
    scheme = engine.generalize(Env.empty(), t_final)
    return Pretty.canonicalize(scheme)
