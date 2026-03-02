"""Test suite for Algorithm W.

Each test builds an expression by hand, runs inference, and checks the
result against an expected type string.

Run with:  python -m pytest python/tests/
       or:  pytest python/tests/
"""
from __future__ import annotations

import pytest

from ..infer import InferError, infer_expr, pp_error
from ..pretty import pp_scheme
from ..types import App, LBool, LInt, Lam, Let, LitExpr, Var

# ---------------------------------------------------------------------------
# Helper constructors (make AST-building less noisy)
# ---------------------------------------------------------------------------


def lam(x: str, e: object) -> Lam:
    """lam("x", e)  ≡  Lam("x", e)"""
    return Lam(x, e)


def v(x: str) -> Var:
    """v("x")  ≡  Var("x")"""
    return Var(x)


def app(f: object, x: object) -> App:
    """app(f, x)  ≡  App(f, x)  (left-associative)"""
    return App(f, x)


def lett(x: str, e1: object, e2: object) -> Let:
    """lett("x", e1, e2)  ≡  Let("x", e1, e2)"""
    return Let(x, e1, e2)


def int_(n: int) -> LitExpr:
    return LitExpr(LInt(n))


def bool_(b: bool) -> LitExpr:
    return LitExpr(LBool(b))


# ---------------------------------------------------------------------------
# Assertion helper
# ---------------------------------------------------------------------------


def should_infer(expr: object, expected: str) -> None:
    """Assert that expr infers to the expected type string.

    Pass expected="error" to assert that inference raises an error.
    """
    try:
        scheme = infer_expr(expr)
        got = pp_scheme(scheme)
        if expected == "error":
            pytest.fail(f"Expected error, but got: {got}")
        else:
            assert got == expected, f"Expected: {expected!r}\nGot:      {got!r}"
    except (Exception,) as e:
        # Check if it's an InferError (UnboundVariable or UnificationError)
        from ..infer import UnboundVariable, UnificationError

        if isinstance(e, (UnboundVariable, UnificationError)):
            if expected != "error":
                pytest.fail(f"Expected: {expected!r}\nGot error: {pp_error(e)}")
        else:
            raise


# ---------------------------------------------------------------------------
# Phase 1 & 2: Literals
# ---------------------------------------------------------------------------


class TestLiterals:
    def test_int_literal(self):
        should_infer(int_(42), "Int")

    def test_bool_literal(self):
        should_infer(bool_(True), "Bool")


# ---------------------------------------------------------------------------
# Phase 3: Lambda / application / let
# ---------------------------------------------------------------------------


class TestLambdaAppLet:
    def test_identity(self):
        # λx. x  :  ∀'a. 'a -> 'a
        should_infer(lam("x", v("x")), "∀'a. 'a -> 'a")

    def test_identity_applied_to_int(self):
        # (λx. x) 42  :  Int
        should_infer(app(lam("x", v("x")), int_(1)), "Int")

    def test_const(self):
        # λx. λy. x  :  ∀'a 'b. 'a -> 'b -> 'a
        should_infer(lam("x", lam("y", v("x"))), "∀'a 'b. 'a -> 'b -> 'a")

    def test_apply_function_to_int(self):
        # λf. f 1  :  ∀'a. (Int -> 'a) -> 'a
        should_infer(lam("f", app(v("f"), int_(1))), "∀'a. (Int -> 'a) -> 'a")

    def test_let_id_apply_to_int(self):
        # let id = λx. x in id 1  :  Int
        should_infer(lett("id", lam("x", v("x")), app(v("id"), int_(1))), "Int")

    def test_let_id_return_id(self):
        # let id = λx. x in id  :  ∀'a. 'a -> 'a
        should_infer(lett("id", lam("x", v("x")), v("id")), "∀'a. 'a -> 'a")

    def test_let_chain(self):
        # let f = id in let g = f in g  :  ∀'a. 'a -> 'a
        should_infer(
            lett("f", lam("x", v("x")), lett("g", v("f"), v("g"))),
            "∀'a. 'a -> 'a",
        )


# ---------------------------------------------------------------------------
# Phase 3: Polymorphism milestones
# ---------------------------------------------------------------------------


class TestPolymorphismMilestones:
    def test_id_used_at_int_then_bool(self):
        # let id = λx. x in let a = id 1 in id true  :  Bool
        # Using id at Int does not prevent using it at Bool.
        should_infer(
            lett(
                "id",
                lam("x", v("x")),
                lett("a", app(v("id"), int_(1)), app(v("id"), bool_(True))),
            ),
            "Bool",
        )


# ---------------------------------------------------------------------------
# Classic combinators
# ---------------------------------------------------------------------------


class TestClassicCombinators:
    def test_compose(self):
        # λf. λg. λx. f (g x)  :  ∀'a 'b 'c. ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
        should_infer(
            lam("f", lam("g", lam("x", app(v("f"), app(v("g"), v("x")))))),
            "∀'a 'b 'c. ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b",
        )

    def test_apply_twice(self):
        # λf. λx. f (f x)  :  ∀'a. ('a -> 'a) -> 'a -> 'a
        should_infer(
            lam("f", lam("x", app(v("f"), app(v("f"), v("x"))))),
            "∀'a. ('a -> 'a) -> 'a -> 'a",
        )

    def test_second_projection(self):
        # λx. λy. y  :  ∀'a 'b. 'a -> 'b -> 'b
        should_infer(lam("x", lam("y", v("y"))), "∀'a 'b. 'a -> 'b -> 'b")

    def test_s_combinator(self):
        # λf. λg. λx. f x (g x)  :  ∀'a 'b 'c. ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
        should_infer(
            lam(
                "f",
                lam(
                    "g",
                    lam("x", app(app(v("f"), v("x")), app(v("g"), v("x")))),
                ),
            ),
            "∀'a 'b 'c. ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c",
        )

    def test_apply(self):
        # λx. λy. x y  :  ∀'a 'b. ('a -> 'b) -> 'a -> 'b
        should_infer(
            lam("x", lam("y", app(v("x"), v("y")))),
            "∀'a 'b. ('a -> 'b) -> 'a -> 'b",
        )

    def test_identity_applied_to_itself(self):
        # (λx. x) (λx. x)  :  ∀'a. 'a -> 'a
        should_infer(
            app(lam("x", v("x")), lam("x", v("x"))),
            "∀'a. 'a -> 'a",
        )


# ---------------------------------------------------------------------------
# Let-polymorphism stress tests
# ---------------------------------------------------------------------------


class TestLetPolymorphism:
    def test_id_applied_to_id(self):
        # let id = λx. x in id id  :  ∀'a. 'a -> 'a
        should_infer(
            lett("id", lam("x", v("x")), app(v("id"), v("id"))),
            "∀'a. 'a -> 'a",
        )

    def test_id_id_1(self):
        # let id = λx. x in id id 1  :  Int
        should_infer(
            lett("id", lam("x", v("x")), app(app(v("id"), v("id")), int_(1))),
            "Int",
        )

    def test_const_through_two_lets(self):
        # let k = λx. λy. x in let k' = k in k'  :  ∀'a 'b. 'a -> 'b -> 'a
        should_infer(
            lett(
                "k",
                lam("x", lam("y", v("x"))),
                lett("k'", v("k"), v("k'")),
            ),
            "∀'a 'b. 'a -> 'b -> 'a",
        )


# ---------------------------------------------------------------------------
# Flip and partial application
# ---------------------------------------------------------------------------


class TestFlipAndPartial:
    def test_flip(self):
        # λf. λx. λy. f y x  :  ∀'a 'b 'c. ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
        should_infer(
            lam("f", lam("x", lam("y", app(app(v("f"), v("y")), v("x"))))),
            "∀'a 'b 'c. ('a -> 'b -> 'c) -> 'b -> 'a -> 'c",
        )

    def test_partial_application_of_const(self):
        # let k = λx. λy. x in k 1  :  ∀'a. 'a -> Int
        should_infer(
            lett("k", lam("x", lam("y", v("x"))), app(v("k"), int_(1))),
            "∀'a. 'a -> Int",
        )

    def test_twice_id_1(self):
        # let twice = λf. λx. f (f x) in let id = λx. x in twice id 1  :  Int
        should_infer(
            lett(
                "twice",
                lam("f", lam("x", app(v("f"), app(v("f"), v("x"))))),
                lett(
                    "id",
                    lam("x", v("x")),
                    app(app(v("twice"), v("id")), int_(1)),
                ),
            ),
            "Int",
        )

    def test_eta_expanded_identity(self):
        # λx. (λy. y) x  :  ∀'a. 'a -> 'a
        should_infer(
            lam("x", app(lam("y", v("y")), v("x"))),
            "∀'a. 'a -> 'a",
        )


# ---------------------------------------------------------------------------
# Shadowing
# ---------------------------------------------------------------------------


class TestShadowing:
    def test_inner_let_shadows_outer(self):
        # let x = 1 in let x = true in x  :  Bool
        should_infer(
            lett("x", int_(1), lett("x", bool_(True), v("x"))),
            "Bool",
        )


# ---------------------------------------------------------------------------
# Expected failures
# ---------------------------------------------------------------------------


class TestExpectedFailures:
    def test_f_f_occurs_check(self):
        # λf. f f  →  occurs-check failure
        should_infer(lam("f", app(v("f"), v("f"))), "error")

    def test_self_application_occurs_check(self):
        # λx. x x  →  occurs-check failure
        should_infer(lam("x", app(v("x"), v("x"))), "error")

    def test_self_application_chained(self):
        # λx. x x x  →  occurs-check failure
        should_infer(lam("x", app(app(v("x"), v("x")), v("x"))), "error")

    def test_apply_non_function(self):
        # 1 true  →  Int is not a function type
        should_infer(app(int_(1), bool_(True)), "error")

    def test_unbound_variable(self):
        # z  →  unbound variable
        should_infer(v("z"), "error")

    def test_lambda_bound_used_at_two_types(self):
        # λf. let _ = f 1 in f true  →  should fail (f is monomorphic)
        should_infer(
            lam(
                "f",
                lett("_", app(v("f"), int_(1)), app(v("f"), bool_(True))),
            ),
            "error",
        )
