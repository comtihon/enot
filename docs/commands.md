# Basic API
To get list of all commands and usage info use:

    enot -h
### create
Generate a new project.

    enot create foobar
will create `./foobar` with content:  
* `src/project_name.app.src` - application src file  
* `src/project_name_app.erl` - application file  
* `src/project_name_sup.erl` - top supervisor  
* `enot_config.json` - enot configuration file

### build
Build a project (in project's dir):

    enot build
This will build a project and put all `beam` files to `ebin` directory.  
If you have `c_src` folder Enot will compile them to `priv/project_name.so`.  
If you have `deps` specified in you config file - they will be downloaded to `deps` and also build.  
`.app` file is generated from `.app.src` with all templates fill in _(see Jinja2 templating)_

### release
To release a project (in project's dir):

    enot release
This will build a project if not built, install [relx](https://github.com/erlware/relx]creates) if not installed,
 create `relx.config`, `rel/vm.args`, `rel/sys.config` if not exist and build a release using relx in `_rel` 
 directory.
 
### version
To get project's version use:

    enot version
This will extract project's version from `enot_config.json`.  
Same command from console:

    cat enot_config.json | jq .app_vsn
    
# Deps API
### deps
Fetch and build deps.

    enot deps
Run this in your project's directory to get all deps fetched, built and linked to project (or just linked, if they
are already in local cache).
### upgrade
Upgrade all `branch` deps to latest version.

    enot upgrade
Run this in your project's directory to move deps commits locks to latest versions. This will work
only for branch, which use `branch` as a source. `tag` deps upgrade automatically.  
Use:

    enot upgrade -d my_dep
to upgrade only `my_dep`. Read [this](deps.md) for more info.

# Package API
### package
Generate a Enot package.
    
    enot package
In `foobar` project's directory will compile foobar project and generate `foobar.ep` Enot package.
This package can be transferred to other machine or uploaded to cache.

### fetch
Fetch Enot package from remote repo to local cache.

    enot fetch foo/bar 1.0.0
Will search all remote repositories for `bar:1.0.0`. If found - will be downloaded and extracted to local
cache.  
Version is not mandatory.  

    enot fetch foo/bar
will fetch latest version of `bar` for namespace `foo`. Namespace is used for distinguish between github forks.   
__Info__: Enot will try find package suitable for you system Erlang version.

### install
Install Enot package.

    enot install foo/bar 1.0.0
Will install package `bar` from local cache (or fetch it first from remote if not exists). Installed package
will be added to list of installed packages.  
Package installation is a step-by-step execution of actions, set in `enot_config.json` `install` section. It is performed
just after fetching and building deps.

### installed
Print all installed packages and versions.

    enot installed

# Tests API
### ct
To run common tests use:

    enot ct
Common test's output is redirected to the console.  
__Important__ Enot supports only tests in `test` directory (subdirectories supported).
### eunit
To run eunit tests use:

    enot eunit
Eunit's test output is redirected to the console.  
__Important__ Enot supports only tests in `test` directory (subdirectories supported).