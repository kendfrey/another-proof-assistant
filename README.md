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
addComm : (u : Level) -> (x : Nat u) -> (y : Nat u) -> Eq u (Nat u) (add u x y) (add u y x) :=
  u => x => y => ?addComm;
```

```
>stack exec another-proof-assistant-exe example.txt
?addComm
u : Level;
x : Nat u;
y : Nat u;
|- Eq u (Nat u) (add u x y) (add u y x)
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
- Proof tactics
- A universe of propositions
- Nat-indexed type universes
  - This was removed as an experiment to test the viability of requiring all definitions to be universe-polymorphic.

## Axioms

- Axiom K holds via the definition of `refl`.
- Propositional extensionality does not hold.
- Quotient soundness is postulated as an axiom.
- Quotient effectivity does not hold in the absence of propositional extensionality.
- Function extensionality holds via quotient types.
- Classical principles such as global choice and the law of excluded middle do not hold.

## Infinitude of primes

A proof of the infinitude of the primes has been formalized in [infinitude-of-primes.txt](infinitude-of-primes.txt). It serves as a demonstration of the language's capabilities, as well as an example of the difficulties that arise without the features of a modern proof assistant.

For example, the proof spans almost 2000 lines of code. In a fully-featured proof assistant, the same proof would likely be less than a tenth of that size.

## Further details

There is much more that could be said about this project, but I couldn't possibly cover everything here. If you have any questions, please ask me and I'll be happy to answer them.