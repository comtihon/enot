# Deps setting
To set deps for your project use `deps` section in `coonfig.json`. If you need
additional deps to be compiled for tests use `test_deps` section for them.  
Deps can be specified by [JSON](http://www.json.org/) objects like this:  

    {
        "name": DepName,
        "url": DepUrl,
        "tag": GitTag / "branch" : BranchName
    }
Where `name` is the name of the dep, `url` its git url. `name` is mandatory, while
`url` can be skipped. If it is skipped - url will be fetched from [hex](https://hex.pm/).  
`tag` is a dep's git tag, which will be fetched and `branch` is a git branch. They
 are mutually exclusive.
# Deps auto updating
When building a project Coon always prefer newer, not major version of a dep. It
assumes all deps use [Semantic](http://semver.org/) Versioning. So, if you have:  

    [
        {"name": "dep1",
         "url": "dep1 url",
         "tag": "1.0.0"},
        {"name": "dep2",
         "url": "dep2 url",
         "tag": "1.0.0"},
        {"name": "dep3",
         "url": "dep3 url",
         "tag": "1.0.0"}
    ]
And `dep2` has:
    
    [
        {"name": "dep3",
         "url": "dep3 url",
         "tag": "1.0.1"}
    ]
Then newer, `1.0.1` version of `dep3` will be used.  
Or, if your project has `1.0.1` version of `dep3` and `dep2` has older `1.0.0` - 
newer version of `dep3` will also be used.  
__Important__ this works only for tag commits. If you specify dep via branch - it will
just overrides all other down level deps.  
__Important__ if you need to turn off deps auto updating - add `{"compare_versions" : false}` to your `coonfig.json`. 
Then coon will take versions closer to root. Beware, using incompatible major versions could be dangerous!
# Deps conflicts
According to semantic versioning - changing major leads to incompatible api changes.
So, if you have different major versions in your deps tree - Coon wont build the 
project:

    [
        {"name": "dep1",
         "url": "dep1 url",
         "tag": "1.0.0"},
        {"name": "dep2",
         "url": "dep2 url",
         "tag": "1.0.0"},
        {"name": "dep3",
         "url": "dep3 url",
         "tag": "2.0.0"}
    ]
And `dep2` has:
    
    [
        {"name": "dep3",
         "url": "dep3 url",
         "tag": "1.0.1"}
    ]
Project won't be built, as dep3 has conflicting major versions. `2.0.0` from root vs
`1.0.1` from `dep2`.
# Deps manual updating
To change a version of a dep - just modify `coonfig.json`:

    [
        {"name": "dep1",
         "url": "dep1 url",
         "branch": "master"},
        {"name": "dep2",
         "url": "dep2 url",
         "tag": "1.0.0"}
    ]
You can modify branch and tag:

    [
        {"name": "dep1",
         "url": "dep1 url",
         "branch": "develop"},
        {"name": "dep2",
         "url": "dep2 url",
         "tag": "1.2.0"}
    ]
This will do following:
* change `dep1` branch from `master` to `develop`
* update `dep2` from `1.0.0` to `1.2.0` tag  

_Hint: {"compare_versions": false} will turn off crash on conflicts._

# Deps auto cleaning
When dep's version changes - it can loose it's own dependencies of gain a new ones. In 
case of new deps - they will be fetched and linked to the project. In case of old - if
nobody uses this dep, it will be deleted:

    [
        {"name": "dep1",
         "url": "dep1 url",
         "branch": "master"},
        {"name": "dep2",
         "url": "dep2 url",
         "tag": "1.0.0"}
    ]
And dep2 has:
    
    [
        {"name": "dep3",
         "url": "dep3 url",
         "tag": "1.0.1"}
    ]
New `dep2` `1.2.0` version no longer uses `dep3`. You don't use it in your dep's tree also.
So it will be unlinked from you project to keep your deps clean.
    
# Deps locks
Coon remembers each commit's hash of branch deps, when you are using them. It creates
`coon_locks.json` in the root of your project where all your deps hashes mentioned. 
This hashes will be used every next time your project is built.  
Coon lets you use fixed branch commits when you develop, test and deploy your project.
Locks guarantee no untested version appears in prod.  
See __Deps upgrade__ for info how to move locks. 

# Deps upgrade
To update your branch deps to newer version you can use `coon upgrade -p some_dep` 
command. It will remove locks for `some_dep` dependency and make it fetch latest version,
setting up new lock. If no package specified via `-p` - all locks will be upgraded.