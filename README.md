# Hindley-Milner Type Inference

A clear, well-tested implementation of **Algorithm W** in Haskell — the classic
type inference algorithm for ML-style languages with let-polymorphism.

**Goal:** deep understanding, not production use. Feed it an AST, get back
inferred types. No parser, no codegen.

**Design:** immutable type variables with explicit substitutions (not mutable
refs), making the algorithm's structure maximally visible.

---

## Quick start

```bash
# Build
just build

# Run tests
just test

# Start a REPL
just repl
```

In the REPL:

```haskell
import Infer
import Types

-- λx. x  →  ∀'a. 'a -> 'a
inferExpr (Lam "x" (Var "x"))

-- let id = λx. x in id 1  →  Int
inferExpr (Let "id" (Lam "x" (Var "x")) (App (Var "id") (Lit (LInt 1))))
```

---

## What it implements

| Feature | Status |
|---|---|
| Lambda calculus (Var, App, Lam) | ✓ |
| Let-polymorphism | ✓ |
| Int / Bool literals | ✓ |
| Occurs check | ✓ |
| Pretty-printed errors | ✓ |
| Canonicalized type variable names | ✓ |

---

## Source layout

```
src/
├── Types.hs    -- Core data types: Expr, Ty, Scheme, Subst
├── Subst.hs    -- Substitution: apply, compose, free type vars
├── Unify.hs    -- Unification with occurs check
├── Infer.hs    -- Algorithm W: infer, instantiate, generalize
├── Env.hs      -- Type environment (name → scheme)
└── Pretty.hs   -- Pretty-printing types and expressions

test/
└── TestInfer.hs -- 30+ hspec test cases
```

---

## The algorithm

Algorithm W works by case analysis on the expression being typed:

- **Var x** — look up the scheme, *instantiate* it with fresh variables. Each
  use of a let-bound name gets its own copy of the quantified variables.
- **Lam x e** — introduce a fresh type variable for `x`, infer `e` in the
  extended environment.
- **App f x** — infer `f`, infer `x` (in the updated environment), unify `f`'s
  type with `argType → freshResult`. Substitution threading here is where bugs
  hide.
- **Let x e1 e2** — infer `e1`, *generalize* its type over the current
  environment to produce a scheme, then infer `e2` with `x` bound to that
  scheme. This is where polymorphism is created.

The two key operations:

- **Instantiate** — `∀a b. a → b → a` becomes `t1 → t2 → t1` with fresh vars.
- **Generalize** — quantify over type vars in `t` that are *not* free in the
  environment (i.e. not still being constrained by an outer inference).

---

## Example type judgments

```
λx. x                           ∀'a. 'a -> 'a
λf. λx. f x                     ∀'a 'b. ('a -> 'b) -> 'a -> 'b
let id = λx. x in (id 1)        Int
let id = λx. x in id            ∀'a. 'a -> 'a   -- polymorphic at use site
λf. (f 1, f true)               error            -- HM has no rank-2 types
λx. x x                         error            -- occurs check
```

---

## Dev tasks

```bash
just build       # compile everything
just test        # run the test suite
just check       # build + test (CI)
just repl        # GHCi with library loaded
just watch       # live-reload on save (requires ghcid)
just lint        # hlint
just fmt         # format with ormolu
just fmt-check   # check formatting without modifying
just clean       # remove build artifacts
```

---

## References

- Damas & Milner, *Principal type-schemes for functional programs* (1982) — the original paper, short and readable
- Grabmüller, *Algorithm W Step by Step* — a worked Haskell implementation
- Pierce, *Types and Programming Languages* ch. 22 — textbook treatment
