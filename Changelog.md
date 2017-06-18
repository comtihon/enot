# 0.\*.\* -> 1.0.0

* git branch is handled differently than git tag. Now commit hash is added to branch name when working with cache.  
F.e.: package with `master` branch will be stored in cache as `master-<CommitHash>`.  
* added `upgrade` command to use latest git commit in a branch
* locks added. When dep is fetched via branch - it's commit is memorized and used further.
* just branch deps, cached later, are no longer used, so you can remove them.