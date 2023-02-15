# another-proof-assistant

This project is a rudimentary dependently typed language and proof assistant.

## Syntax

Lambda expressions

```
f => g => x => f (g x)
```

Function types

```hs
A -> B
```

Dependent function types

```hs
(x : A) -> B x
```

Top-level definitions

```hs
id : (u : Level) -> (A : Type u) -> A -> A := _ => _ => x => x;
```

Holes

```hs
Nat : (u : Level) -> Type u := u => ?TODO;
```

See [funext.txt](funext.txt) for a comprehensive example.

## Primitive types

- `Empty` The type with 0 elements
- `Unit` The type with 1 element
- `Bool` The type with 2 elements
- Dependent functions (unnamed, uses special syntax)
- `Sigma` Dependent pairs
- `Level` Polymorphic universe indexes
- `Type` Types in a polymorphic universe
- `Typew+n` Types in a non-polymorphic universe (display only, not referenceable)
- `Eq` The equality/identity type
- `W` The W type, for constructing arbitrary inductive types
- `Quot` The quotient type

## Notable non-features

- Implicit arguments & type classes
- Pattern matching
- Inductive type definitions
- Nat-indexed type universes
  - This was removed as an experiment to test the viability of requiring all definitions to be universe-polymorphic.

## Axioms

- Axiom K holds via the definition of `refl`.
- Propositional extensionality does not hold.
- Quotient soundness is postulated as an axiom.
- Quotient effectivity does not hold in the absence of propositional extensionality.
- Function extensionality holds via quotient types.
- Classical principles such as global choice and the law of excluded middle do not hold.