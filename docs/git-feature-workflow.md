# Git Workflow for feature branches

The Acton repository primarily adheres to the triangular git workflow, where
feature branches are used to introduce all changes and these are merged to the
main branch through Pull Requests (PRs).

Feature branches are well suited to small fixes and well scoped out features
where we know about the scope up front.

## Starting work on a new feature / bug fix

- You should branch out your feature branch from the `main` branch
- First ensure your local `main` branch is up to date with the remote
- Give your branch a good name
  - If there is a GitHub issue for it, you can use the issue number as the
    prefix, like if you are fixing a bug described in issue #1337, the branch
    can be called `1337-fix-foobar-bug`
  - Don't sweat it too much, you can always rename the branch later
    - to rename, checkout the feature branch, then do `git branch -m
      1337-fix-blargh-bug` to rename

Branch out feature branch:
```
git checkout main
git pull
git checkout -b 1337-fix-foobar-bug
# do some changes
git commit ...
git push -u origin 1337-fix-foobar-bug
```
- Open a Pull Request on GitHub to merge your feature branch into `main`
- GitHub CI tests will run for the branch on GitHub and if it passes, the PR can
  be merged
  - If tests do not pass, chances are there is a test that needs to be marked as
    not failing anymore or similar. Check CI test output or try running tests
    locally!
- If GitHub says there are conflicts during merge, do NOT attempt to solve them
  in the web browser. Abort!
  - Fix conflicts by rebasing your feature branch on main, see below.
  
## Rebasing your local feature branch

- If the `main` branch has had updates since you branched out your feature
  branch, you might get a conflict when trying to merge your feature branch back
  into `main`
- Fix this by rebasing your feature branch on the new main

```
git checkout main
git pull
git checkout 1337-fix-foobar-bug
git rebase main
# sort out potential conflicts
git push --force origin 1337-fix-foobar-bug
```

The last step of pushing the branch is sort of optional, but if you have already
opened a PR as per the above workflow, then you will need to use `--force` when
pushing after a rebase, since you have rewritten git history.

## Cleaning up old local feature branches

Once you have merged your feature branch(es) upstream, you can remove them
locally. First we pull down the `main` branch to ensure we see if things are
merged. We use `--prune` which also updates information about remote branches
and in particular, will remove (prune!) them if they no longer exist.

```
git checkout main
git pull --prune origin
```

Now you can go through your branches and remove them with `git branch -d
1337-fix-foobar-bug`. If the branch is fully merged to `main`, there won't be
any output, like so:

```
kll@Boxy:~/terastream/acton$ git branch -d regression-actor-method-call-not-async 
Deleted branch regression-actor-method-call-not-async (was 1a34a28).
kll@Boxy:~/terastream/acton$ 
```

If the branch is not merged to `main` nor pushed to the remote, an error is
raised:
```
kll@Boxy:~/terastream/acton$ git branch -d add-git-develop-workflow-cheat-sheet 
error: The branch 'add-git-develop-workflow-cheat-sheet' is not fully merged.
If you are sure you want to delete it, run 'git branch -D add-git-develop-workflow-cheat-sheet'.
kll@Boxy:~/terastream/acton$ 
```

If the branch is pushed to the remote but not yet merged to main, a warning will
be emitted:
```
kll@Boxy:~/terastream/acton$ git branch -d warnings-as-errors 
warning: deleting branch 'warnings-as-errors' that has been merged to
         'refs/remotes/origin/warnings-as-errors', but not yet merged to HEAD.
Deleted branch warnings-as-errors (was 60703e6).
kll@Boxy:~/terastream/acton$ 
```

You can then "undo" the branch removal by checking it out again, which will
default to checking out what exists on the remote:
```
kll@Boxy:~/terastream/acton$ git checkout warnings-as-errors 
Branch 'warnings-as-errors' set up to track remote branch 'warnings-as-errors' from 'origin'.
Switched to a new branch 'warnings-as-errors'
kll@Boxy:~/terastream/acton$ 
```
