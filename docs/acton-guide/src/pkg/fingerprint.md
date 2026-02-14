# Fingerprint and lineage

Each Acton project must declare a **fingerprint** in `Build.act` (or `build.act.json`). The fingerprint represents the project’s **lineage** — a stable identity that stays the same across releases of the same project.

```python
name = "myproject"
fingerprint = 0x1234abcd5678ef00
```

## What the fingerprint is

- A 64-bit hex value (`0x...`).
- The upper 32 bits are derived from the project name.
- The lower 32 bits are random.
- Acton uses this lineage to deduplicate dependencies and to generate consistent build metadata.

## Rename vs fork

Renaming a project breaks lineage, so generate a new fingerprint for the new name.
Forking a project also creates a new lineage, so generate a fresh fingerprint.

If Acton detects a mismatch, it will fail the build and tell you to generate a new fingerprint for the name.

## Current behavior

- `name` and `fingerprint` are required in every project.
- Acton validates the lineage prefix derived from the name.
- Missing or mismatched values fail the build with guidance.
