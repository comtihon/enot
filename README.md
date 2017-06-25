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

### Project Coon Configuration
Coon configuration file is `coonfig.json`, it is placed in project_dir. It is in [JSON](http://www.json.org) format.

    {
        "name" : AppName,
        "fullname" : Namespace/AppName
        "erlang" : [ListOfSupportedReleases],
        "app_vsn" : AppVsn,
        "with_source" : Boolean,
        "link_all" : Boolean,
        "rescan_deps" : Boolean,
        "deps" : [
            {
                "name" : DepName,
                "url" : DepUrl,
                "tag" : GitTag / "branch" : BranchName
            }
        ],
        "test_deps" : [
            {
                "name" : DepName,
                "url" : DepUrl,
                "tag" : GitTag / "branch" : BranchName
            }
        ],
        "auto_build_order" : Boolean,
        "override" : Boolean,
        "compare_versions" : Boolean,
        "disable_prebuild" : Boolean,
        "prebuild" : [
            {Action : Params}
        ],
        "build_vars" : [
            {Var1 : Value}
            Var2
        ],
        "c_build_vars" : [
            {VarName1 : VarValue1}
        ]
    }
Here:  
__name__ is the name of current project.  
__fullname__ is a namespace with name. It is used to distinguish github forks. Usually you don't need it, as it will set 
up automatically from url.  
__erlang__ is a list of erlang releases you application is compatible with. Is used by [octocoon](https://github.com/comtihon/octocoon)
build system.  
__app_vsn__ is a version of erlang application. Coon uses it when composing `.app` and in `relx.conf`.  
__with_source__ if set to true - will include source, when moving to local/remote cache and packaging. Default is `true`.  
__link_all__ if set to true - all deps will be linked to main project (including deps of deps). Default is `true`. You can
  set it to false, to reduce number of deps you are not using directly in your project, but keep in mind, that relx 
  needs all dep tree for proper building. So if you use releases - switch to true.  
__rescan_deps__ if set to true (default is `true`) - will rescan all deps tree if dep update detected after build - and 
remove dead deps from deps directory. You can set it to false if you prefer manual deps removing.  
__deps__ is a list of deps, where `dep.name` is a name of dep, `dep.url` is a full url to dep. If it is not specified - 
url will be fetched from [hex](https://hex.pm/), `dep.tag` is a tag of a dep and `dep.branch` is a branch. 
Last two are mutually exclusive.    
__test_deps__ is the same, that `deps`, but are built, fetched and linked only for ct/eunit.  
__auto_build_order__ when true - searches project's source files content for parse-transform usage. If parse-transform 
module belongs to the same repo - will compile it first. Default is `true`. Can be set to `false` to speed up 
compilation.  
__override__ if set to true - root project will override deps tree build configuration, such as `build_vars`, 
`c_build_vars` and `disable_prebuild`. Default is `false`. Pay attention, that this won't work in case of `native` or 
`makefile` build in Coon Global Config.  
__compare_versions__ if set to `false` - will not fail on incompatible (based on major vsn) deps are used in project. 
Default is `true`.  
__disable_prebuild__ if set to true - Coon won't execute any prebuild steps. It is useless without `override` set to true
in case you don't wont to execute deps prebuild steps, may be for security reasons.  
__prebuild__ is a list of actions, which should be run before build. `prebuild.Action` is a type of the action. 
Only `shell` is supported now. `prebuild.Params` are the options to be passed to action. TODO example here.  
__build_vars__ is a list of erlang build vars, used when building a project. They can be either single or with value. 
Build vars can also be passed via coon argument `--define`. It can be used in case of test builds, if you don't want 
to add test build vars to project conf.  
__c_build_vars__ is a list of build vars, used when building `c_src` sources.  

_Why JSON?_  
it is simple, well known, and can be easily accessed by third-party tools:

    cat coonfig.json | jq .
    <nice color output>
    cat coonfig.json | jq .app_vsn
    "3.0.0"

### Other build systems
Although it is not recommended, but you can use [Erlang.mk](https://erlang.mk/) or `rebar.config` as a project config. 
For now only version, name and deps can be used.

### Dependency management
* speficy deps in coonfig
* deps update if tag changed in coonfig
* deps auto update if newer dep presents in dependency tree
* deps auto removing if dep is not used any more  

More about [dependency management](../docs/deps.md).

### Jinja2 templating
Coon allows you to use [Jinja2](http://jinja.pocoo.org/) template engine in your config files:  
__app.src__

    {application, project_name, [
        {description, ""},
        {vsn, "{{ app.vsn }}"},
        {registered, []},
        {applications, {{ app.std_apps + app.apps }}},
        {modules, {{ modules }}},
        {mod, {project_name_app, []}},
        {env, []}
    ]}.
Where `modules` are the list of all application's modules and `app` is the object of Package class from 
`coon/packages/package.py` representing current project. You can use it's properties in templating.  
In `app.src` you have also `hostname` available for templating.  
More about [templating](../docs/templating.md).   

__relx.config__

    {release, {"{{ app.name }}", "{{ app.vsn }}"}, ["{{ app.name }}"]}.
    {sys_config, "rel/sys.config"}.
    {vm_args, "rel/vm.args"}.
    {extended_start_script, true}.
Where `app` is the object of Package class from `coon/packages/package.py` representing current project.    
You can add `relx.config`, `vm.args` and `sys.config` to git as templates. When running release - these templates will be
 filled, but after the release files will be overwritten as before. So, no files will be changed.

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