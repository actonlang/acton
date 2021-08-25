# Git Workflow for the `develop` branch

The Acton repository primarily adheres to the triangular git workflow, where
feature branches are used to introduce all changes and these are merged to the
main branch through Pull Requests (PRs).

The `develop` branch is an exception. It is not a feature branch, rather it is
long lived. Development happens there for improvements to the compiler and other
parts that might not be ready to merge in standalone and where it is still
useful for others to be able to see (and test) these improvements.

Long lived branches come with certain challenges, in particular how to keep it
up to date with the `main` branch. Here's a quick cheat sheet:

## Merging things on `develop` branch to `main`

- Before attempting to merge in the commits on `develop` into `main`, it is a
  good idea to ensure it is up to date with the `main` branch
  - We accomplish this through **rebasing**.
  - First we have to make sure the local `main` branch is up to date with the
    remote at GitHub.
- After rebase, we need to force push (since we have rewritten the git log).
- There might be conflicts during rebasing that need to be sorted out manually.

```
git checkout main
git pull
git checkout develop
git rebase main
# maybe sort out conflicts
git push --force develop
```

- Open a Pull Request on GitHub to merge `develop` into `main`
- GitHub CI tests will run for the `develop` branch on GitHub and if it passes,
  the PR can be merged
  - If tests do not pass, chances are there is a test that needs to be marked as
    not failing anymore or similar. Check CI test output or try running tests
    locally!
- If GitHub says there are conflicts during merge, do NOT attempt to solve them
  in the web browser. Abort!
  - Instead, repeat the above procedure. Ensure that you pull the main branch so
    your local branch is up to date with the remove!
- There is an option in the PR merge to remove the `develop` branch - deselect
  this option since we want the `develop` branch to remain.

## Updating local `develop` branch from GitHub remote

If person #1 runs the above workflow of rebasing the `develop` branch on `main`
in order to bring it up to date, then others who use the `develop` branch will
need to update it as well. Since git history has been rewritten, some extra
steps need to be taken.

- Ensure git merge only accepts fast forward merges!
  - This ensures linear history on `develop` by avoiding merge commits
  - Do this by adding config to your `~/.gitconfig`:
```
[pull]
	ff = only
```
- To update your local `develop` branch with the remotes:
```
git checkout develop
git pull --rebase --force
```
- Any local commits not yet pushed will be rebased upon the remote changes,
  after this you can push them to the remote with: `git push`
