# Diagnose Vendor Notes

Baseline: upstream `mesabloo/diagnose` commit `d3ffbb7f376ddc64f3f349fe3a6a8e49d405cde0`
(`master`, package version `2.5.1`).

This baseline is newer than the Hackage `diagnose-2.5.1` release. It includes
upstream PR #24, which removes the direct `text` dependency instead of forcing
consumers onto `text <=2.0`.

Maintenance policy:

- Treat this directory as a forkable upstream package, not project-specific code.
- Keep local changes generic and suitable for a standalone `diagnose` fork or
  upstream pull request.
- Document each local patch here with its rationale.
- Prefer separate commits for the upstream baseline import and each local patch.
- If local changes grow beyond small dependency/build fixes, extract this
  directory to a dedicated fork and pin this build to that repository commit.

Local patches:

- None beyond vendoring this upstream snapshot and this note.
