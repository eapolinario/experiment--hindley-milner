# Hindley-Milner Type Inference Engine

## Project Overview

Building a complete Algorithm W type inference engine from scratch in OCaml (or Haskell) for a small ML-like lambda calculus with let-polymorphism. The goal is deep learning — not a production compiler, but a clear, well-tested implementation that makes the algorithm's structure transparent.

**Scope:** AST-in, typed-AST-out. No parser, no codegen. Feed it an AST, get back inferred types.

**Timeline:** ~2 weeks (medium-effort side project).

**Design choice:** Immutable type variables with explicit substitutions (not mutable refs). Purer, easier to reason about, makes the algorithm structure very visible.

---

## Architecture

```
src/
├── types.ml          -- Core data types: expr, ty, scheme, subst
├── subst.ml          -- Substitution: apply, compose, free type vars
├── unify.ml          -- Unification with occurs check
├── infer.ml          -- Algorithm W: infer, instantiate, generalize
├── env.ml            -- Type environment (name -> scheme mapping)
├── pretty.ml         -- Pretty-printing types and expressions
└── tests/
    └── test_infer.ml -- Test suite
```

---

## Core Data Types

### Expressions (input AST)
- `Var of string` — variables
- `App of expr * expr` — application
- `Lam of string * expr` — lambda abstraction
- `Let of string * expr * expr` — let-polymorphism (the key HM feature)
- `Lit of lit` — integer/bool literals (for testing convenience)

### Types (what inference produces)
- `TVar of tvar` — type variables (get unified)
- `TCon of string` — concrete types: `int`, `bool`
- `TFun of ty * ty` — function arrow `a → b`

### Type Schemes (for polymorphism)
- `Scheme of tvar list * ty` — universally quantified type, e.g. `∀a. a → a`

### Substitution
- A map from type variables to types (`tvar -> ty`)
- Operations: `apply`, `compose`, `ftv` (free type variables)

---

## Phased Roadmap

### Phase 1: Core data types (Day 1)
- Define `expr`, `ty`, `scheme`, `subst` types
- Implement fresh type variable generation (global counter or state monad)
- Implement `ftv` (free type variables) for types, schemes, and environments
- Implement pretty-printing early — you'll need it for debugging everything else

### Phase 2: Substitution and unification (Days 2–4)
This is the mathematical heart.

**Substitution:**
- `apply : subst -> ty -> ty` — walk a type, replacing vars
- `compose : subst -> subst -> subst` — chain two substitutions (order matters!)
- Lift `apply` to work on schemes and environments

**Unification (`unify : ty -> ty -> subst`):**
- Two identical type vars → empty subst
- Type var vs type → bind (after occurs check)
- Two `TFun` → unify args, apply resulting subst, unify return types
- Mismatch → type error

**Key tests:**
- `unify (a → b) (int → int)` → `{a ↦ int, b ↦ int}`
- `unify a (a → b)` → occurs check failure
- `unify (a → a) (int → bool)` → error

**Common bug:** Getting substitution composition order wrong. `compose s1 s2` should apply `s1` to the range of `s2`, then merge.

### Phase 3: Algorithm W (Days 5–8)
`infer : env -> expr -> (subst, ty)`

Case analysis on the expression:

- **Var x** — Look up scheme in env, **instantiate** (replace quantified vars with fresh vars). Each use of a let-bound variable gets fresh copies — this is where polymorphism happens.
- **Lam x e** — Fresh type var `a` for `x`, infer `e` in extended env `{x: a}`, return `(subst, a → inferred_type)`.
- **App f x** — Infer `f` → `(s1, t1)`. Infer `x` in `s1(env)` → `(s2, t2)`. Fresh var `b` for result. Unify `s2(t1)` with `t2 → b` → `s3`. Return `(s3 ∘ s2 ∘ s1, s3(b))`. **Substitution threading here is where bugs hide.**
- **Let x e1 e2** — Infer `e1` → `(s1, t1)`. **Generalize** `t1` over `s1(env)` to get a scheme. Infer `e2` in extended env with `x` bound to that scheme. Return composed subst.

**Two critical operations:**
- **Instantiate:** `∀a b. a → b → a` becomes `t1 → t2 → t1` with fresh vars
- **Generalize:** quantify over type vars in `ty` that are NOT free in the environment

**Milestone tests:**
- `λx. x` infers `a → a`
- `let id = λx. x in (id 1, id true)` typechecks (polymorphic `id`)
- `λf. (f 1, f true)` fails (no first-class polymorphism in HM)
- `let f = λx. x in let g = f in g` infers polymorphic type

### Phase 4: Error messages (Days 9–12)
- Attach source locations to AST nodes, propagate through inference
- When unification fails, report which expression generated the conflicting constraint
- Detect and explain infinite types from occurs-check failures
- Consider tracking constraint origins: "this constraint arose from applying f to x at line N"

### Phase 5: Extensions and polish (Days 13–14)
Pick one or two:
- **Recursive let** (`let rec`): bind `x` to a fresh var *before* inferring `e1`, then unify
- **If/then/else**: unify condition with `bool`, unify both branches, good exercise
- **Tuples / pairs**: extend `ty` with `TProd of ty * ty`
- **Pattern matching**: significant jump in complexity, but very rewarding
- **A comprehensive test suite** of classic examples

---

## Key References

- Damas & Milner, "Principal type-schemes for functional programs" (1982) — the original, short and readable
- Grabmüller, "Algorithm W Step by Step" — a Haskell tutorial / worked implementation
- Heeren, Hage & Swierstra, "Generalizing Hindley-Milner Type Inference Algorithms" — for the constraint-based perspective
- Pierce, "Types and Programming Languages" ch. 22 — good textbook treatment

---

## Notes

- Prefer clarity over performance. This is a learning project.
- Test-driven: write expected type judgments before implementing each phase.
- When debugging, pretty-print substitutions at every step of `infer`.
- The substitution threading in `App` is the #1 source of bugs. Double-check the order.
