# Actors & concurrency

Actors are the center of Acton's model for mutable state and
concurrency. If you want to understand how Acton programs stay
structured as they grow, start here.

<div class="beginner-content">
<p>Start with the basic <code>actor main(env)</code> pattern. The core
model is simple: an actor owns state, handles one message at a time,
and communicates with other actors by calling their methods.</p>
</div>

This is also the closest Acton replacement for Rust-style lifetime
thinking: actors own mutable state, capability references are passed
explicitly, and the runtime manages ordinary object lifetime.

<div class="advanced-content">
<p>The important guarantee is local seriality: within one actor, state
changes are observed as if messages were handled one at a time. That is
why <code>var</code> can hold mutable state without introducing the kind
of shared-memory races that make threaded code hard to reason about.
The actor boundary is doing real work here: mutation stays local, while
concurrency appears only in the relationships between actors.</p>

<p>Viewed that way, many Acton features are the same model in different
forms. Sync and async calls, <code>await</code>, delayed callbacks with
<code>after</code>, and lifecycle hooks all control when new work is
placed into an actor's mailbox and when another actor is allowed to make
progress. What Acton does not promise is one global timeline across the
whole program. Ordering is local to each actor unless your own protocol
establishes something stronger.</p>
</div>

This section covers:

- how actors own state and methods
- how the root actor starts a program
- how actors talk to each other
- how delayed work and cleanup fit into actor code
- how concurrency works without shared mutable memory

Read these pages next:

- [Actors](../actors.md)
- [Root Actor](../actors/root.md)
- [Lifetime](../actors/lifetime.md)
- [Attributes](../actors/attributes.md)
- [`self`](../actors/self.md)
- [Actor methods](../functions/actor_methods.md)
- [Sync Method calls](../actors/sync_method_call.md)
- [Async Method calls](../actors/async_method_call.md)
- [Control flow in an async actor world](../control_flow/actors.md)
- [after / sleep](../control_flow/after_sleep.md)
- [Concurrency](../actors/concurrency.md)
- [Cleanup](../actors/cleanup.md)

Effect annotations such as `pure`, `mut`, `proc`, and `action` are
documented under [Working with types](../types/effects.md).
