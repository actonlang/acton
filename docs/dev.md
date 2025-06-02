# Development Workflow n stuff

## Changelog

We keep a `CHANGELOG.md`. It is good practice for PRs to contain a changelog
entry for whatever they are updating. This is often forgotten and so in
practice, we often update `CHANGELOG.md` in batches. When making a release,
carefully inspect the git log and update `CHANGELOG.md` accordingly. Match the
style and tone in `CHANGELOG.md`. For long descriptive git commit messages or
whole branches with multiple commits, write a shorter summary. If the git commit
messages are very brief, inspect the code and attempt to sum it up.


## Git workflow

- authoritative git repository is https://github.com/actonlang/acton
  - git clone git@github.com:actonlang/acton.git
- `main` is the main branch
- the `main` branch is protected, meaning no one can directly push commits to it
- commits should happen on a branch, from which a Pull Request (PR) is created
- the PR can then be merged to `main` via GitHub
- all PRs should contain a changelog entry in `CHANGELOG.md` under the
  `Unreleased` heading
  - this is later used as the body of the release

## Release workflow

There are two types of releases, "version releases" and the `tip` release.

### Versioned release
- this is like version `1.2.3`
- create a new branch called `release-v` followed by the version number, like
  `release-v1.2.3`
- prepare `CHANGELOG.md` by placing the content currently under the `Unreleased`
  heading under a version number, like version `1.2.3` instead
- push branch to GitHub and open PR
- once the PR is merged, a GitHub Action workflow will take over, create a tag
  and proceed to build the final release build and upload as an artifact
  - it will take the content from `CHANGELOG.md` and use as the GitHub Release
    notes
  - artifacts from the CI test jobs will be included in the Release as assets
  - the Release is published at https://github.com/actonlang/acton/releases

### `tip` release
- this is the binary output from the last successful CI job on the `main`
  branch
  - somewhat like a nightly, just more frequent or less - depends on if there is
    new content on the `main` branch or not
  - `tip` will identify themselves with the base version number and the date
    and time when it was built
    - for example, if v0.4.0 was the last versioned release and a nightly is
      built, it would be called 0.4.0.2021.08.05.09.27.14, i.e. it was built at
      09:27:14 on the 5th of August 2021
  - this is automatically built for every merge on `main`, no extra action is
    required
  - the output is stored as a pre-release in Github called `tip`
    - new releases will overwrite older ones, i.e. only the latest build will be
      available
  - the release has a stable URL:
    https://github.com/actonlang/acton/releases/tag/tip
