# Rebase Workflow
When working with git, I like to use a rebase workflow in order to keep all the history of changes consolidated and clean. However, there are a few annoyances that come from this workflow:
## Fixing old commits can be a pain
To fix this, previously I would do an interactive rebase to make the change on individual commits. This works but can get quite annoying:
```sh
git rebase -i HEAD~3
# Mark commit to change with an i then make changes.
```

This workflow still works if there are lots of things to change but it's easy to get lost. Instead, git has added a fixup flag that will automate a good chunk of this process. To use it, first make the changes that you want to add to an old commit. Then use the following commands:
```sh
git commit --fixup=<commit>
git commit --fixup :/second # Will add a fixup commit pointing to the most recent commit message containing the word `second`
```
This will create a new fixup commit right after the specified commit. This new commit will contain the changes you want for this commit. Then, either right away or after you've made several changes, you can run the following to consolidate all the changes into the respective commits:
```sh
git rebase --autosquish
```
Now all the commits will look how you want.

## Updating long chains of branches can be a pain.
Sometimes when developing features using github or gitlab, you may have large chains of branches that depend on each other, each acting as a reviewable change or individual feature for a fast moving project:
```txt
feature 3
 ^
 |
 |
feature 2
 ^
 |
 |
feature 1
 ^
 |
 |
main
```
Now, lets say main gets updated and you want to rebase onto it so all your features are all still properly pointing to each other and the new main branch, you'll want to rebase all of them. Previously, this often required tediously going through each one and rebasing, however it turns out you can just use the following to do all this automatically:
```sh
git checkout "feature 3" # Will only update branches under the current branch
git rebase --update-refs main
```
now all the features should be properly rebased.
