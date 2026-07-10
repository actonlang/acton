# Output Artifacts

Acton treats source as the canonical form of a package. Dependencies are
identified by the content hash of their source. For large packages, Acton can
use a prebuilt output artifact instead of rebuilding the dependency from
source. If no matching artifact is available, Acton falls back to fetching and
building the source locally.

An output artifact contains the package's compiled type interfaces, not
generated code. Producing an artifact only runs the compiler front end (parse
and type check); code generation is deferred to the consuming build, which
generates code from the artifact for the parts it uses.

Acton uses OCI repositories for artifact distribution. OCI is the image and
artifact format used by container registries. Acton takes advantage of that
registry infrastructure, which is widely available across hosting providers,
CI systems, and private deployments.

OCI is the transport and storage convention, not the source of artifact
identity. Artifacts are identified by the content hash of the source, the Acton
interface version, and the current target tuple. The source content hash
remains the stable identity; OCI tags are only the lookup convention.

## Install oras

Acton uses the `oras` command-line tool to pull and publish OCI artifacts.
Install `oras` and make sure it is on your `PATH` before using output
artifacts.

A build can still fall back to source when `oras` is not installed. An artifact
that is already available in the local dependency cache can also be used without
contacting any registry. Publishing artifacts requires `oras`.

## Using artifacts

At build time, Acton knows the artifact identity it is looking for: the
dependency source content hash, interface version, and target tuple. It searches
OCI repositories for an artifact with that identity.

Those repositories can be published in two common places. Package authors can
publish artifacts near their source repository, and Acton can derive those
locations for some repository URLs. A team can also run its own artifact
repository as a shared cache, populated from packages that may only publish
source.

To search a shared artifact repository, pass it during the build:

```bash
acton build --artifact-repo local-registry.local-domain.com/acton-out
```

The artifact repository is the full OCI image name without the tag. The path
can include whatever namespace, group, or organization layout your registry
uses. Acton appends a tag derived from the dependency source content hash and
interface version:

```text
<source-hash>-iface<interface-version>
```

For example, Acton may expand the artifact repository above to:

```text
oci://local-registry.local-domain.com/acton-out:N-V-__8AAJqWJQA7T6BgHCyrFsXB4bMcBA4_qmIOxqVaL8Vm-iface0.18
```

You can search more than one artifact repository by repeating the option:

```bash
acton build \
  --artifact-repo local-registry.local-domain.com/acton-out \
  --artifact-repo backup-registry.local-domain.com/platform/cache/acton-out
```

Once an artifact has been pulled into the dependency cache, later plain
`acton build` runs reuse it without repeating `--artifact-repo`.

Acton checks the local dependency cache before contacting any registry. If a
matching artifact is available locally, Acton checks that it matches the
requested source content hash, interface version, and target before using it.

Passing `--artifact-repo` opts the build into artifact consumption: a valid
artifact is preferred even when a cached source entry already exists, and the
cached source is used only when no artifact can be found. This matters on a
machine that has published the artifact itself, since producing seeds the
caches with the package source, which would otherwise always win over the
artifact. Without `--artifact-repo`, a cached source entry is used as is,
without probing remote artifact repositories.

For local testing or offline workflows, `--artifact-repo` can also point to an
OCI image layout directory:

```bash
acton build --artifact-repo /tmp/acton-out
```

Local OCI layouts are queried before remote repositories and do not require a
registry service.

For dependency repository URLs on GitHub or GitLab, Acton can derive a
producer-side OCI repository to try automatically:

- GitHub `https://github.com/OWNER/REPO` maps to
  `oci://ghcr.io/OWNER/REPO/acton-out:<source-hash>-iface<interface-version>`.
- GitLab `https://gitlab.com/GROUP/PROJECT` maps to
  `oci://registry.gitlab.com/GROUP/PROJECT/acton-out:<source-hash>-iface<interface-version>`.

If the artifact is missing or incompatible, Acton falls back to the source
package.

An artifact also carries enough package metadata for the build planner to know
the project name, fingerprint, and transitive dependency graph.

## Publishing artifacts

To publish an artifact from a package checkout:

```bash
acton artifact push --repo-url https://github.com/OWNER/REPO
```

This command derives the conventional OCI reference from the repository URL and
uses `oras` to push the artifact. To publish to another registry or local OCI
layout, provide the artifact repository without a tag:

```bash
acton artifact push --artifact-repo local-registry.local-domain.com/acton-out
acton artifact push --artifact-repo ghcr.io/OWNER/REPO/acton-out
acton artifact push --artifact-repo /tmp/acton-out
```

`acton artifact push` type checks the current package before packaging and
pushing it. No code is generated in this step; the artifact ships the compiled
type interfaces and the consuming build performs code generation. `acton
artifact pack` does the same check-and-pack step but only writes the artifact
locally:

```bash
acton artifact pack
```

The artifact is packaged from the local checkout. Acton computes the source
content hash from the package source after the check step, and the artifact tag
is derived from that hash. Consumers need matching source content to use the
artifact.

To print the hash without building or publishing:

```bash
acton artifact hash
```

## Validation and trust

The source content hash is the artifact identity. The OCI lookup key is derived
from that identity and Acton's compatibility metadata, so a matching lookup is
expected to return the artifact for that source content.

Acton still records the same identity inside the artifact and checks it before
use. This is a sanity check against stale uploads, manual registry mistakes, and
incompatible compiler outputs. It is not a separate binary signing scheme.

Remote artifact pulls use the registry transport, normally HTTPS with TLS. When
Acton derives an artifact repository from a dependency `repo_url`, the registry
namespace follows the source repository owner. For example, a GitHub dependency
maps to `ghcr.io/OWNER/REPO/acton-out`, and a GitLab dependency maps to
`registry.gitlab.com/GROUP/PROJECT/acton-out`.

For custom `--artifact-repo` values, Acton cannot derive that ownership
relationship. Trust in those repositories has to be established out of band.
