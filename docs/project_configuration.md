# Enot Project configuration (Recommended)
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
        ],
        "install" : [
            {Action: Params}
        ],
        "uninstall" : [
            {Action: Params}
        ]
    }
Here:  
__name__ is the name of current project.  
__fullname__ is a namespace with name. It is used to distinguish github forks. Usually you don't need it, as it will set 
up automatically from url.  
__erlang__ is a list of erlang releases you application is compatible with. Is used by [Enot Auto Builder](https://github.com/comtihon/enot_auto_builder)
build system.  
__app_vsn__ is a version of erlang application. Enot uses it when composing `.app` and in `relx.conf`.  
__with_source__ if set to true - will include source, when moving to local/remote cache and packaging. Default is `true`.  
__link_all__ if set to true - all deps will be linked to main project (including deps of deps). Default is `true`. You can
  set it to false, to reduce number of deps you are not using directly in your project, but keep in mind, that relx 
  needs all dep tree for proper building. So if you use releases - switch to true.  
__rescan_deps__ if set to true (default is `true`) - will rescan all deps tree if dep update detected after build - and 
remove dead deps from deps directory. You can set it to false if you prefer manual deps removing.  
__deps__ is a list of deps. Read more in [deps](deps.md) section.    
__test_deps__ is the same, that `deps`, but are built, fetched and linked only for ct/eunit.  
__auto_build_order__ when true - searches project's source files content for parse-transform usage. If parse-transform 
module belongs to the same repo - will compile it first. Default is `true`. Can be set to `false` to speed up 
compilation.  
__override__ if set to true - root project will override deps tree build configuration, such as `build_vars`, 
`c_build_vars` and `disable_prebuild`. Default is `false`. Pay attention, that this won't work in case of `native` or 
`makefile` build in Enot Global Config.  
__compare_versions__ if set to `false` - will not fail on incompatible (based on major vsn) deps are used in project. 
Default is `true`.  
__disable_prebuild__ if set to true - Enot won't execute any prebuild steps. It is useless without `override` set to true
in case you don't wont to execute deps prebuild steps, may be for security reasons.  
__prebuild__ is a list of actions, which should be run before build. `Action` is a type of the action. 
Only `shell` is supported now. `prebuild.Params` are the options to be passed to action. TODO example here.  
__build_vars__ is a list of erlang build vars, used when building a project. They can be either single or with value. 
Build vars can also be passed via enot argument `--define`. It can be used in case of test builds, if you don't want 
to add test build vars to project conf.  
__c_build_vars__ is a list of build vars, used when building `c_src` sources.  
__install__ is a list of actions to be performed on `enot install` for your package. See Install steps for more info.
__uninstall__ is a list of actions to be performed on `enot unnstall` your package.  

_Why JSON?_  
it is simple, well known, and can be easily accessed by third-party tools:

    cat enot_config.json | jq .
    <nice color output>
    cat enot_config.json | jq .app_vsn
    "3.0.0"
    
## Install steps
### Shell step
Perform ordinary shell command.   
Example:

    "install": [
            {"shell": "id -u example_user &>/dev/null || useradd example_user"}
         ]
Will create `example_user` if it does not exist.
### Release step
Build an Erlang release in specified directory.  
Example:

    "install": [
            {"release": {"rel_dir" : "/opt"}}
         ]
Will download all deps, erts for the package being installed, compose Erlang release and put everything into `/opt/_rel`.
    
# Rebar/Erlang.mk Project configuration
Although it is not recommended, but you can use [Erlang.mk](https://erlang.mk/) or `rebar.config` as a project config. 
For now only version, name and deps can be used.  
Example `rebar.config`:

    {deps, [
              {dep1, ".*", {git, "git://github.com/comtihon/dep1"}},
              {dep2, ".*", {git, "git://github.com/comtihon/dep2", {branch, "master"}}},
              {dep3, ".*", {git, "git://github.com/comtihon/dep3", ""}},
              {dep4, ".*", {git, "git://github.com/comtihon/dep4", {tag, "1.0.0"}}},
              {dep5, ".*", {git, "git://github.com/comtihon/dep5", "commit_hash"}},
              {dep6, ".*", {git, "git://github.com/comtihon/dep6", {ref, "commit_hash"}}}
            ]}.
or:
    
    {deps, [
              {dep1, {git, "git://github.com/comtihon/dep1"}},
              {dep2, {git, "git://github.com/comtihon/dep2", {branch, "master"}}},
              {dep3, {git, "git://github.com/comtihon/dep3", ""}},
              {dep4, {git, "git://github.com/comtihon/dep4", {tag, "1.0.0"}}},
              {dep5, {git, "git://github.com/comtihon/dep5", "commit_hash"}},
              {dep6, {git, "git://github.com/comtihon/dep6", {ref, "commit_hash"}}}
            ]}.
or from hex:
    
    {deps, [
               {hex_dep, "1.0.0"}
           ]}
__Important__: When parsing Erlang.MK config Enot can't distinguish between tags and deps.
