# Scope

Review of the current `docs/acton-guide` working tree, with focus on
`src/language.md`, the new `src/language/*` pages, nearby `types`,
`actors`, `protocols`, and `stdlib` splits, `src/SUMMARY.md`, and
`theme/detail-toggles.css` where the new skill-level behavior affects how
the guide is read.

# High-level Assessment

The rewrite is moving the guide toward a clearer, task-first structure.
The landing pages are more human-oriented than the old chapter stubs, and
the beginner/advanced layering is a real improvement in several places.
The main problems are in the seams: optional handling is split across two
places without enough navigation context, the built-in vs standard
library boundary is still fuzzy, and the new skill-level UI is not fully
usable on narrow screens.

# Findings

1. `docs/acton-guide/src/SUMMARY.md:32-35`, `docs/acton-guide/src/language/optionals_none.md`, and `docs/acton-guide/src/types/optionals.md` create a confusing split for optionals. Readers see both `Optionals and None` and `Optionals`, but only one of them is obviously the basic entry point. The two pages overlap enough that the hierarchy reads as duplication instead of a deliberate beginner-to-reference progression.

2. `docs/acton-guide/src/stdlib.md:5-16` overstates what the new built-in/reference split contains. It says this section covers built-in definitions such as core types and built-in protocols, but the navigation only exposes protocols and imported modules. Core types still live under `docs/acton-guide/src/primitives.md` in the Language chapter, so the new stdlib landing page does not actually own the whole built-in surface it describes.

3. `docs/acton-guide/theme/detail-toggles.css:60-65` makes the detail-level system inaccessible on mobile. The skill control is hidden below 600px, but beginner and advanced callouts are hidden by default and only revealed through that control. Because many of the rewritten language pages now put substantive explanation into those blocks, narrow-screen readers are locked to whatever skill level was last saved.

4. `docs/acton-guide/src/primitives/tuples.md:7-22` introduces named-field tuple syntax without enough setup. The page jumps from positional tuple access to `point = (x=3, y=4)` and `point.x` with no explanation of whether this is a named tuple, row, or some other Acton-specific shape. That makes the built-in types chapter feel less self-contained than the rest of the new Language structure.

# Remediation Plan

1. Make the optionals path explicit in `SUMMARY.md` by distinguishing the beginner page from the type-system page, or collapse the overlap into one clearer sequence.
2. Either add a built-ins landing page for core types or reword `stdlib.md` so it only claims ownership of what the nav actually exposes.
3. Keep the skill selector reachable on narrow screens, or make the default page content sufficient without requiring the hidden control.
4. Add a short introduction to named tuple/row syntax before using named-field tuple examples, or switch the example to plain tuple syntax until that concept is covered.
