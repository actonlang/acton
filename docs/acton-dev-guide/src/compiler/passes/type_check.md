# Type check

The type checker follows a Hindley-Milner style and borrows the protocol model from Swift.
Implementation lives in `compiler/lib/src/Acton/Types.hs` and `compiler/lib/src/Acton/TypeEnv.hs`.

The type checker runs on top of an import-augmented environment produced by
`Acton.Env.mkEnv`, not directly on the scheduler's raw module graph. See
[Imports and environments](../imports_and_envs.md) for the environment layers
and how transitive imports are materialized from `.ty` files.
