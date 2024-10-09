# Worktrees
[link](https://git-scm.com/docs/git-worktree)
Worktrees are a feature to allow you to work on multiple branches at one time, just in different folders. Branches can only be checked out by one folder at a time to prevent collisions.

## PR
A PR is one of the biggest use cases I see for this. Being able to checkout code for a peer review without touching any of the code in my main branch sounds wonderful. If there's a peer review on a remote branch, I can check it out in a worktree using this:
```sh
git worktree add <path to temp folder>/PR-1056 origin/PR-1056
```

## Quick bug fix
If I want to make a new folder for handling a bug fix without context switching out of the feature I'm working on (or conversely if I want to work on multiple features in parallel), I can use the following to make a new branch and worktree folder at the same time:
```sh
git wortree add -b <new branch name> <path to branch> main # Uses main branch as foundation
```
