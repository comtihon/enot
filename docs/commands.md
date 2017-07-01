# Basic API
To get list of all commands and usage info use:

    coon -h
### create
Generate a new project.

    coon create foobar
will create `./foobar` with content:  
* `src/project_name.app.src` - application src file  
* `src/project_name_app.erl` - application file  
* `src/project_name_sup.erl` - top supervisor  
* `coonfig.json` - coon configuration file

### build
Build a project (in project's dir):

    coon build
This will build a project and put all `beam` files to `ebin` directory.  
If you have `c_src` folder Coon will compile them to `priv/project_name.so`.  
If you have `deps` specified in you config file - they will be downloaded to `deps` and also build.  
`.app` file is generated from `.app.src` with all templates fill in _(see Jinja2 templating)_

### release
To release a project (in project's dir):

    coon release
This will build a project if not built, install [relx](https://github.com/erlware/relx]creates) if not installed,
 create `relx.config`, `rel/vm.args`, `rel/sys.config` if not exist and build a release using relx in `_rel` 
 directory.
 
### version
To get project's version use:

    coon version
This will extract project's version from `coonfig.json`.  
Same command from console:

    cat coonfig.json | jq .app_vsn
    
# Deps API
### deps
Fetch and build deps.

    coon deps
Run this in your project's directory to get all deps fetched, built and linked to project (or just linked, if they
are already in local cache).
### upgrade
Upgrade all `branch` deps to latest version.

    coon upgrade
Run this in your project's directory to move deps commits locks to latest versions. This will work
only for branch, which use `branch` as a source. `tag` deps upgrade automatically.  
Use:

    coon upgrade -d my_dep
to upgrade only `my_dep`.  
Read [this](deps.md) for more info.

# Package API
### package
Generate a Coon package.
    
    coon package
In `foobar` project's directory will compile foobar project and generate `foobar.cp` Coon package.
This package can be transferred to other machine or uploaded to cache.

### add_package
Add Coon package to remote repo.

    coon add_package my_remote_repo
Loads previously created Coon package to remote repo. Remote repo credentials should exist in 
global Coon configuration. Package will be loaded with username from configuration as a namespace. Use 
[octocoon](https://github.com/comtihon/octocoon) integration to add package with it's own namespace.  
By default try to find package in current dir. You can specify `--package` argument to point to package's
file.  
Other arguments: 
__--rewrite__ - rewrite package if already exists.  
__--recurse__ - add all dep's tree recursively to repo. Default is True. __Info__: octocoon integration 
doesn't load package's deps.

### fetch
Fetch Coon package from remote repo to local cache.

    coon fetch foo/bar 1.0.0
Will search all remote repositories for `bar:1.0.0`. If found - will be downloaded and extracted to local
cache.  
Version is not mandatory.  

    coon fetch foo/bar
will fetch latest version of `bar` for namespace `foo`. Namespace is used for distinguish between github forks.   
__Info__: Coon will try find package suitable for you system Erlang version.

### install
Install Coon package.

    coon install foo/bar 1.0.0
Will install package `bar` from local cache (or fetch it first from remote if not exists). Installed package
will be added to list of installed packages

### installed
Print all installed packages and versions.

    coon installed

# Tests API
### ct
To run common tests use:

    coon ct
Common test's output is redirected to the console.  
__Important__ Coon supports only tests in `test` directory (subdirectories supported).
### eunit
To run eunit tests use:

    coon eunit
Eunit's test output is redirected to the console.  
__Important__ Coon supports only tests in `test` directory (subdirectories supported).