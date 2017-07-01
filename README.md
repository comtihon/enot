# Coon 
[![Build Status](https://travis-ci.org/comtihon/coon.svg?branch=master)](https://travis-ci.org/comtihon/coon)
[![PyPI](https://img.shields.io/pypi/v/coon.svg)](https://pypi.python.org/pypi/coon)
[![PyPI](https://img.shields.io/pypi/pyversions/coon.svg)](https://pypi.python.org/pypi/coon)
[![PyPI](https://img.shields.io/pypi/wheel/coon.svg)](https://pypi.python.org/pypi/coon)  
Erlang advanced project manager.  
_Why Coon?_  
- powerful dependency management
- built deps caching locally and remotely
- json project configuration
- Jinja2 templating
- automatic files build order  

Be in touch: [Blog](https://justtech.blog/tag/coon/)

### Installation
Pypi:

    pip install coon
Manual:
    
    git clone https://github.com/comtihon/coon.git
    make install

### Create project
Generate a project:

    coon create project_name
This will create `project_name` directory with content:  
* `src/project_name.app.src` - application src file  
* `src/project_name_app.erl` - application file  
* `src/project_name_sup.erl` - top supervisor  
* `coonfig.json` - coon configuration file

### Build
Build a project (in project's dir):

    coon build
This will build a project and put all `beam` files to `ebin` directory.  
If you have `c_src` folder Coon will compile them to `priv/project_name.so`.  
If you have `deps` specified in you config file - they will be downloaded to `deps` and also build.  
`.app` file is generated from `.app.src` with all templates fill in _(see Jinja2 templating)_

### Release
To release a project (in project's dir):

    coon release
This will build a project if not built, install [relx](https://github.com/erlware/relx]creates) if not installed,
 create `relx.config`, `rel/vm.args`, `rel/sys.config` if not exist and build a release using relx in `_rel` 
 directory.
 
### Package management api
To learn how to fetch, install, uninstall and list coon packages read [commands](docs/commands.md).

### Project Configuration
Coon configuration file is `coonfig.json`, it is placed in project_dir. It is in [JSON](http://www.json.org) format.
Coon also supports `rebar.config` and `Makefile` ErlangMK's config.  
For format, fields and examples read [project configuration](docs/project_configuration.md).

### Dependency management
* speficy deps in coonfig
* deps update if tag changed in coonfig
* deps auto update if newer dep presents in dependency tree
* deps auto removing if dep is not used any more  

More about [dependency management](docs/deps.md).

### Jinja2 templating
Coon allows you to use [Jinja2](http://jinja.pocoo.org/) template engine in your `app.src`, `relx.config`, `vm.args` 
and `sys.config`.  
After filling templates and making a release all file changes are reverted. Coon's files templates won't disturb you 
with endless git changes.
For more information read [templating](docs/templating.md).   

### Caching
Every dependency, fetched and built locally is saved to coon local cache. It is situated in `user_cache_dir/coon`: 
`$HOME/.cache/coon` for Linux. Cache path can be changed via coon global config. Cached applications are divided by 
namespace, project and erlang version, which was used in their compilation.  
For example:
    
    {
        "name" : "mongodb-erlang",
        "url" : "https://github.com/comtihon/mongodb-erlang",
        "tag" : "v3.0.1"
    }
Will be saved to `$HOME/.cache/coon/comtihon/mongodb-erlang/v3.0.1/20`, where path contains static path to cache 
`$HOME/.cache/coon/` it can be specified in Coon global config. Dynamic path - `Namespace/Project/Tag/Erlang_version`.  
Every time same version of Erlang and project will be used as dep in another project on this system - dep will be linked
 from cache to this project instead of downloading and compiling new.  
There is also remote cache, where already built packages are kept. Coon searches packages in remote cache before cloning
them from git and building. Remote cache can be set in Coon global config.   
Besides using official remote Coon cache (TODO Coon doesn't have official remote cache!) you can deploy your own remote 
cache. Currently Coon supports [Artifactory](https://www.jfrog.com/artifactory/) and [Amazon S3](https://aws.amazon.com/s3/)
TODO s3 not supported :(  
You can use multiple remote caches.

### Global Coon Configuration
Coon global configuration is system wide Coon configuration file. It is in JSON format also and stored in
`user_config_dir/coon`: `$HOME/.config/coon` for Linux.

    {
      "compiler" : "coon",
      "temp_dir": "/tmp/coon",
      "cache":
      [
        {
          "name": "local",
          "type": "local",
          "url": "file://$HOME/.cache/coon"
        }
      ]
    }
Where:
`compiler` is a default build system, which can be used to built projects.  
Options are: `coon`, `erlang.mk`, `rebar`, `rebar3`, `native` (will determine compiler by content of the project, f.e. 
if there is `rebar.config` - will use `rebar`), `makefile` (just run `make`), `bootstrap` (just run `./bootstrap`).  
`temp_dir` is the system temp dir. It is used for downloading deps when building the project, before adding them to 
local cache.  
`cache` is a list of caches. Each cache has its own configuration:  
`cache.name` is a name of the cache, which should be unique. It is for Coon only.  
`cache.type` is a type of the cache. Options are: `local`, `artifactory`, `s3`.  
`cache.url` is a url of cache. Local caches use `file://` as a protocol.  
In case of artifactory additional fields are:  
`cache.username` - artifactory username  
`cache.password` or `cache.api_key` - artifactory access credentials.  
In cace of S3 cache additional fields are:
TODO

### Unit testing
Put your unit tests in `test` folder (Coon support subdirectories) and run `coon eunit`. Eunit output will be redirected
to std output. In case of error Coon's return code will be 1.

### Common  testing
Put your common tests in `test` folder (Coon support subdirectories) and run `coon ct`. Common test's output will be
redirected to std output. In case of error Coon's return code will be 1.  
__--log-dir__ parameter can be specified to set up dir for `logs` where ct output is stored. Default if `test/logs`.

### Coon Testing 
To test Coon itself, clone this repo and run.
    
    python setup.py install
All:

    make tests
Single Testcase:

    py.test -q -s test/test_module.py::TestClass
Single Test

    py.test -q -s test/test_module.py::TestClass::test_fun
For artifactory tests artifactory service should be running locally on 8081 with `admin`:`password` credentials and 
`example-repo-local` repo.