# Enot 
[![Build Status](https://travis-ci.org/comtihon/enot.svg?branch=master)](https://travis-ci.org/comtihon/enot)
[![PyPI](https://img.shields.io/pypi/v/enot.svg)](https://pypi.python.org/pypi/enot)
[![PyPI](https://img.shields.io/pypi/pyversions/enot.svg)](https://pypi.python.org/pypi/enot)
[![PyPI](https://img.shields.io/pypi/wheel/enot.svg)](https://pypi.python.org/pypi/enot)  
Erlang advanced project manager.  
_Why Enot?_  
- powerful dependency management -> build stability
- built deps caching locally and remotely -> increased build speed
- json project configuration -> devOps and third-party tools are happy
- Jinja2 templating -> dynamic environment-dependent configuration
- automatic files build order -> no problems with parse-transform
- easy deployment via [EnotHub](https://enot.justtech.blog) -> run `enot install your_service` on every machine
- notes on [security](docs/packaging_and_security.md).  

Be in touch: [Blog](https://justtech.blog/tag/enot/)

### Installation
Pypi:

    pip install enot
Manual:
    
    git clone https://github.com/comtihon/enot.git
    make install

### Create project
Generate a project:

    enot create project_name
This will create `project_name` directory with content:  
* `src/project_name.app.src` - application src file  
* `src/project_name_app.erl` - application file  
* `src/project_name_sup.erl` - top supervisor  
* `enot_config.json` - enot configuration file

### Build
Build a project (in project's dir):

    enot build
This will build a project and put all `beam` files to `ebin` directory.  
If you have `c_src` folder Enot will compile them to `priv/project_name.so`.  
If you have `deps` specified in you config file - they will be downloaded to `deps` and also build.  
`.app` file is generated from `.app.src` with all templates fill in _(see Jinja2 templating)_
#### Build speed-up
Reproduce:
```
git clone https://github.com/comtihon/mongodb-erlang
cd mongodb-erlang
time make

cd ../ && rm -rf mongodb-erlang && git clone https://github.com/comtihon/mongodb-erlang
cd mongodb-erlang
time enot build

cd ../ && rm -rf mongodb-erlang && git clone https://github.com/comtihon/mongodb-erlang
cd mongodb-erlang
time enot build
```
Results:
```
time make
real    0m8,407s

(no local cache) time enot build
real    0m6,609s

(with local cache) time enot build
real    0m1,357s
```
For small project with 3 deps enot is 2 seconds faster for the first time and 7 seconds faster for the second time.

### Release
To release a project (in project's dir):

    enot release
This will build a project if not built, install [relx](https://github.com/erlware/relx]creates) if not installed,
 create `relx.config`, `rel/vm.args`, `rel/sys.config` if not exist and build a release using relx in `_rel` 
 directory.
 
### Package management api
To learn how to fetch, install, uninstall and list enot packages read [commands](docs/commands.md).

### Project Configuration
Enot configuration file is `enot_config.json`, it is placed in project_dir. It is in [JSON](http://www.json.org) format.
Enot also supports `rebar.config` and `Makefile` ErlangMK's config.  
For format, fields and examples read [project configuration](docs/project_configuration.md).

### Dependency management
* speficy deps in enot_config
* deps update if tag changed in enot_config
* deps auto update if newer dep presents in dependency tree
* deps auto removing if dep is not used any more  

More about [dependency management](docs/deps.md).

### Jinja2 templating
Enot allows you to use [Jinja2](http://jinja.pocoo.org/) template engine in your `app.src`, `relx.config`, `vm.args` 
and `sys.config`.  
After filling templates and making a release all file changes are reverted. Enot's files templates won't disturb you 
with endless git changes.
For more information read [templating](docs/templating.md).   

### Caching
Every dependency, fetched and built locally is saved to enot local cache. It is situated in `user_cache_dir/enot`: 
`$HOME/.cache/enot` for Linux. Cache path can be changed via enot global config. Cached applications are divided by 
namespace, project and erlang version, which was used in their compilation.  
For example:
    
    {
        "name" : "mongodb-erlang",
        "url" : "https://github.com/comtihon/mongodb-erlang",
        "tag" : "v3.0.1"
    }
Will be saved to `$HOME/.cache/enot/comtihon/mongodb-erlang/v3.0.1/20`, where path contains static path to cache 
`$HOME/.cache/enot/` it can be specified in Enot global config. Dynamic path - `Namespace/Project/Tag/Erlang_version`.  
Every time same version of Erlang and project will be used as dep in another project on this system - dep will be linked
 from cache to this project instead of downloading and compiling new.  
There is also remote cache, where already built packages are kept. Enot searches packages in remote cache before cloning
them from git and building. Remote cache can be set in Enot global config.   
Besides using official remote Enot cache - [EnotHub](https://enot.justtech.blog) you can deploy your own remote cache. 
You can use multiple remote caches.

### Global Enot Configuration
Enot global configuration is system wide Enot configuration file. It is in JSON format also and stored in
`user_config_dir/enot`: `$HOME/.config/enot` for Linux.

    {
      "compiler" : "enot",
      "temp_dir": "/tmp/enot",
      "cache":
      [
        {
          "name": "local",
          "type": "local",
          "url": "file://$HOME/.cache/enot"
        }
      ]
    }
Where:
`compiler` is a default build system, which can be used to built projects.  
Options are: `enot`, `erlang.mk`, `rebar`, `rebar3`, `native` (will determine compiler by content of the project, f.e. 
if there is `rebar.config` - will use `rebar`), `makefile` (just run `make`), `bootstrap` (just run `./bootstrap`).  
`temp_dir` is the system temp dir. It is used for downloading deps when building the project, before adding them to 
local cache.  
`cache` is a list of caches. Each cache has its own configuration:  
`cache.name` is a name of the cache, which should be unique. It is for Enot only.  
`cache.type` is a type of the cache. Options are: `local` and `enot`.  
`cache.url` is a url of cache. Local caches use `file://` as a protocol.  

### Unit testing
Put your unit tests in `test` folder (Enot support subdirectories) and run `enot eunit`. Eunit output will be redirected
to std output. In case of error Enot's return code will be 1.

### Common  testing
Put your common tests in `test` folder (Enot support subdirectories) and run `enot ct`. Common test's output will be
redirected to std output. In case of error Enot's return code will be 1.  
__--log-dir__ parameter can be specified to set up dir for `logs` where ct output is stored. Default if `test/logs`.

### Enot Testing 
To test Enot itself, clone this repo and run.
    
    python setup.py install
All:

    make tests
Single Testcase:

    py.test -q -s test/test_module.py::TestClass
Single Test

    py.test -q -s test/test_module.py::TestClass::test_fun