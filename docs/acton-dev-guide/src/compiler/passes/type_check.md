# Type check

The type checker follows a Hindley-Milner style and borrows the protocol model from Swift.
Implementation lives in `compiler/lib/src/Acton/Types.hs` and `compiler/lib/src/Acton/TypeEnv.hs`.

The type checker runs on top of an import-augmented environment produced by
`Acton.Env.mkEnv`, not directly on the scheduler's raw module graph. See
[Imports and environments](../imports_and_envs.md) for the environment layers
and how transitive imports are materialized from `.tydb` files.

## Witness environment

Protocol evidence is represented by `Witness` values. A witness stores both the
implementing type (`wtype`) and the implemented protocol (`proto`), but compiler
generated witnesses are expected to be free of `TUni` subterms. The current
construction paths support that invariant:

- primitive witnesses are fixed `WInst` values over effect types
- extension witnesses are `WClass` values built from `NExt` records
- type-variable witnesses are `WInst` values whose `wtype` is a `TVar`
- extension-instance witnesses are `WInst` values whose `wtype` is a `TCon`

The related `QBind` values are also expected to be `TUni`-free. Parsed and
kind-generated binders have no way to contain `TUni`, `selfQuant` creates only
`TVar` arguments, and generalization substitutes solved unification variables
with fresh `TVar`s before building protocol bounds. These are compiler-state
invariants, not guarantees enforced by the raw data constructors.

### Active and closed witnesses

`TypeX` currently stores separate active and closed witness lists and maps.
Substitution is not the fundamental reason for that split: if `Witness` and
`QBind` are `TUni`-free, `usubst` is the identity on witnesses. The useful
distinction is lifetime. Witnesses introduced by `tydefineVars` are scoped to
the corresponding quantified type variables, and `limitQuant` removes the
leading `WInst` witnesses whose `wtype` is one of the outgoing `TVar`s. Closed
witnesses can include primitive `WInst`s, but not `WInst`s with a `TVar`
`wtype`, so applying the same removal predicate to a combined witness list is
equivalent to applying it only to the active list and then appending the closed
list.

### Enumeration order

Witness enumeration order is semantically important for solver candidate
search, even though exact witness lookup should not choose between alternatives
by order. The intended order is the textual order produced by expanding imports
inline depth-first and then scanning matching definitions top to bottom.
`allBelowProto` uses `witsByPName` in that order to produce candidate types for
`Solver.solve`; `tryAlts` then tries those candidates in order. This ordering is
a solver search principle, not a witness-overlap resolution rule.

Direct `Proto` reduction is different. `findWitness` is used as an exact lookup
for a protocol/type pair, and the solver only consumes the result when there is
exactly one matching witness. Multiple matches are not resolved by picking the
first or last witness.

The existing witness maps use `Seq` buckets to preserve oldest-first order while
supporting cheap append as new witnesses are added. This avoids either
append-at-end cost for plain oldest-first lists or a full reverse when querying
newest-first lists. The laziness benefit is limited in solver candidate search,
because `condense` intersects candidate lists and may force much of the input,
but `Seq` still captures the desired insertion-order representation.

### Possible split

The witness environment serves two separate purposes and could use separate
indexes:

- protocol enumeration: `protocol -> witnesses`, in textual order, for
  `allBelowProto` and related candidate generation
- exact lookup: `(type head, protocol) -> witness status`, for `findWitness`
  and `hasWitness`

An exact lookup index should represent zero, one, or ambiguous matches directly.
It also needs a key form for non-`TCon`/`TVar` heads such as primitive effect
types, or those rare cases must keep using protocol enumeration. Imported
witnesses still require the existing alias/unaliased deduplication semantics:
two witnesses are the same when their protocol `tcname` and `wtype` match.
