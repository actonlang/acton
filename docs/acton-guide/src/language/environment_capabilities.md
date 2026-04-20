# Environment and capabilities

Programs that talk to the outside world need explicit access. In
Acton, that access comes through the root `env` actor and through
capability references that are passed around like any other value.

<div class="beginner-content">
<p>Most small programs start with <code>env</code> and end with
<code>env.exit(0)</code>. That is normal. The important part is that
<code>env</code> is passed in explicitly. When a function needs access
to the outside world, give it the specific capability or environment
reference it needs instead of assuming that access is always there.</p>
</div>

This chapter covers the practical side of that model:

- [Security](../security.md) explains why access is explicit.
- [Environment](../environment.md) covers `env`, arguments, variables,
  stdin, and terminal mode changes.

Use this chapter when you need to:

- read command line arguments
- inspect or change environment variables
- read from standard input
- handle interactive terminal input
- decide what authority a helper should receive

<div class="advanced-content">
<p>Capability design is part of API design. When a helper takes a wide
environment reference or a broad outside-world capability, that choice
becomes part of the helper's contract: callers must now trust it with
everything that capability can do, not just the one operation the
current implementation happens to use.</p>

<p>A narrower capability does more than look tidy. It limits authority,
reduces the amount of code that must be audited when security matters,
and makes substitution easier in tests or alternate runtimes. If a
helper only needs to open a TCP connection, that is the capability it
should receive. Anything wider increases coupling and makes accidental
authority leaks more likely over time.</p>
</div>
