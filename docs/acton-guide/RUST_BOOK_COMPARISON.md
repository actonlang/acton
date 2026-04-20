# Rust Book Comparison

## Scope

This comparison reviews the current `docs/acton-guide` working tree
against the Rust book's beginner-to-reference arc, but it judges the two
guides by topic shape and teaching progression rather than by literal
chapter order.

The goal is to identify where the Acton guide already teaches the right
idea, where it compresses several Rust-book ideas into one Acton-specific
model, and where a learner would still need more scaffolding.

## High-Level Assessment

The Acton guide is more task-oriented and runtime-model-first than the
Rust book. It does a good job covering everyday syntax, collections,
optionals, exceptions, classes, protocols, actors, testing, and package
workflows. Its strongest trait is that it teaches the Acton model
directly instead of forcing readers through a generic imperative primer.

Compared with the Rust book, the weak point is continuity. Rust tends to
build one mental model at a time and deepen it. The Acton guide often
splits related ideas across several pages or introduces Acton-specific
runtime concerns before the reader has a stable data-modeling baseline.
The largest gaps are the lack of a dedicated simple-data-modeling path,
the absence of a pattern-matching chapter, the fragmented modules and
packages story, and a thinner learner-facing treatment of
higher-order/iterator-style programming.

## Common Programming Concepts

Acton covers the same baseline set as Rust's opening chapter: bindings
and scope, built-in types, expressions, functions, comments, and control
flow. This part of the guide is solid, and it makes a good beginner
foundation.

The key Acton difference is how mutability is framed. Rust introduces
`mut` in a world of ownership and borrowing. Acton instead emphasizes
constant module bindings, actor-local `var`, and the fact that mutable
state belongs inside actors. That is the right Acton-specific move.

The main weakness is sequencing. Several examples already use
`actor main(env)` and other runtime ideas while the reader is still
digesting ordinary programming concepts. The guide tells readers they do
not need to understand actors yet, but the examples still make actors
part of the early mental load.

## Structs and Data Modeling

Rust uses structs early to give beginners a named composite value and a
place to attach methods before moving into richer modeling. Acton does
not have a direct struct chapter.

The nearest pieces are tuples, named tuples, classes, and protocols.
That covers the feature space, but not the teaching arc. Named tuples
appear as a convenience in the primitives section, while classes are
introduced later as the answer when a tuple becomes too anonymous.

What is missing is a dedicated bridge from "small anonymous value" to
"named data shape" to "behavior-bearing object". Without that bridge,
the guide leaves a gap between the tuple examples and the class-based
modeling chapter.

## Enums and Pattern Matching

Rust's enums and `match` chapter has no direct Acton equivalent. Acton
instead teaches optionals and exceptions as the two main ways to model
absence and failure.

That is good coverage for the features Acton actually has, but it is not
a pattern-matching story. This should be treated as a language
difference, not as a missing Rust chapter to recreate mechanically.

The docs should be explicit that there is no Rust-style `match` arc here
and that optionals, conditionals, and exceptions are the intended tools
for the nearby problems.

## Packages, Crates, Modules, and Code Organization

Acton does cover modules, project structure, package management,
dependency fetching, and repository dependency handling. The material is
practical and clear about `src/`, `import`, pinned hashes, local
dependencies, and override workflows.

The weakness is that the story is fragmented across `language`,
`projects`, `modules`, `package_management`, and the `pkg/*` pages.
Rust book presents packages, crates, and modules as one continuous mental
model. Acton's guide is more operational: it explains what to type and
what the build system does, but it does not yet tie the whole sequence
together from single-file program to module tree to dependency graph.

## Common Collections

Acton covers lists, dictionaries, and sets well. The guide is strong on
choosing the right collection, showing the concrete APIs, and keeping
the type-safety story visible. Comprehensions and iteration examples are
also useful.

The gap is not basic coverage but conceptual bridging. The shared
protocol surface behind collections is mostly hidden in the reference
material, so readers see the concrete APIs before they see the shared
model. Rust spends more time building a single story around collection
APIs; Acton would benefit from a stronger bridge from concrete
collection use to the underlying iterable/indexable/mapping protocols.

## Error Handling

Acton has a clean split between absence and failure: optionals model
ordinary absence, and exceptions model real failures. That distinction
is at least as clear as Rust's early `Option`/`Result` split.

The guide also covers narrowing, optional chaining, and forced
unwrapping, which gives the reader several levels of sophistication. The
weak spot is that the decision rule is spread across multiple pages, so
the reader sees the mechanics before the teaching story is fully
settled.

What is missing is a sharper "when do I return `?T`, when do I raise,
and when do I force unwrap?" rule. That choice should be front-loaded
more explicitly.

## Generics, Traits, and Lifetimes

Acton has good coverage for generics and protocol constraints, and its
built-in protocol reference covers much of the same ground Rust traits
do. The guide also explains effects, which is a useful Acton-specific
layer that Rust book does not need in the same form.

Lifetimes are different. Rust needs a standalone lifetime story because
ownership and borrowing are core to the language. Acton does not need
that exact chapter, so this is not a missing feature in the same sense.
What is missing is a beginner-facing explanation of the replacement
model: actor ownership, explicit capability passing, and GC-backed
object lifetime remove the need for a borrow-checker style arc.

In other words, the guide has the pieces, but it does not yet connect
them into a concise "why there is no Rust-style lifetime chapter here"
explanation.

## Testing

Acton's testing coverage is broader than the Rust book's. It goes beyond
unit tests into actor tests, environment tests, snapshots, flaky tests,
performance, and stress testing. That breadth is valuable for the
language.

The tradeoff is that the core lesson is less visible. Rust introduces
testing as a straightforward habit. Acton should keep the unit-test path
front and center and treat the specialized test kinds as extensions of
that base model rather than as the first thing a newcomer has to sort
through.

## Functional Features

Rust's closures and iterators chapter has no direct Acton twin. Acton
does have higher-order functions, comprehensions, and iteration over
collections, but the guide does not yet teach a coherent "functional
style" arc.

The missing pedagogy is especially noticeable around closures, captured
state, and iterator pipelines. Some of that may be less central to Acton
than to Rust, but the current docs still leave the reader without a
strong conceptual bridge from "a function is a value" to "here is how
you build reusable pipelines over data."

## Concurrency

This is where Acton diverges most from Rust book, and that difference is
part of the language design. Rust treats concurrency as a systems topic
layered on ownership and threads. Acton treats actors as the primary
unit of mutable state and concurrency, so the guide naturally teaches
async calls, delayed work, cleanup, and capability boundaries as core
language ideas.

The coverage is good, but the teaching load is heavy. The reader has to
absorb state ownership, communication, and concurrency at the same
time. A little more framing around "sequential inside an actor,
concurrent between actors" would make the section easier to digest.

## OOP Comparison

Rust uses its OOP comparison chapter to explain why objects,
encapsulation, and inheritance are only part of the design space. Acton
already covers classes, initialization, inheritance, and protocols, and
it generally steers readers away from overusing inheritance.

The gap is mostly rhetorical and structural. The guide explains the
pieces, but it does not yet stage the tradeoff as a focused comparison
between class-based design, protocol-based design, and actor-based state
ownership. That would help readers coming from class-heavy languages
understand why Acton's preferred abstractions differ.

## Concrete Gaps

- No dedicated pattern-matching story, so readers never get a Rust-style
  `match` equivalent because the guide does not say whether one exists.
- No single beginner path for "small anonymous value -> named tuple ->
  class", even though the docs use all three ideas.
- Modules, projects, and package dependencies are split across several
  pages, which makes the code-organization story feel fragmented.
- Higher-order functions are present, but closures and
  iterator-style composition are not taught as a first-class learning
  arc.
- Lifetimes are not a real Acton chapter, but the docs do not yet
  explain clearly enough why actor ownership and capabilities replace
  the need for one.
- Testing is broad and useful, but the basic unit-test workflow is
  partially buried under specialized test categories.

## Prioritized Remediation Plan

1. Add a short data-modeling bridge that explains tuples, named tuples,
   classes, and protocols as one progression.
2. Add or rewrite an overview page for modules and package management
   that links source layout, imports, and dependency resolution into one
   mental model.
3. Add a functional-programming overview that connects higher-order
   functions, comprehensions, and iteration protocols, and states
   plainly what Acton does not have compared with Rust closures and
   iterators.
4. Tighten the absence/failure story so the choice between optional
   values, exceptions, and forced unwrapping is stated up front.
5. Add a short note in the type or actor sections explaining that
   Rust-style lifetimes are not a direct Acton concept because actor
   ownership and capability passing solve a different problem.
