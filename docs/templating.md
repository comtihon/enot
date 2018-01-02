# Coon templates in conf files
Coon supports [Jinja2](http://jinja.pocoo.org/) templates in app.src and relx.conf files.  
Pros:
  * package version in one place
  * modules and deps auto including
  * conditional env generation in app.src

# relx.conf templating
When running release Coon can modify your `relx.conf` passing current `Package `
class to it. All `Package` getters, marked with `@property` are available.  
Example:  

    {release, {"{{ app.name }}", "{{ app.vsn }}"}, ["{{ app.name }}"]}.
    {sys_config, "rel/sys.config"}.
    {vm_args, "rel/vm.args"}.
    {extended_start_script, true}.
Where app is an instance of `Package` class.  
For package `dummy` it will produce following:  

    {release, {"dummy", "1.0.0"}, ["dummy"]}.
    {sys_config, "rel/sys.config"}.
    {vm_args, "rel/vm.args"}.
    {extended_start_script, true}.
# app.src templates
When running build Coon can also use templates to convert app.src -> app. It can use environment vars and
`modules`, `app`, `hostname`, `erl` as special vars. Modules is the list of all compiled module names,
`hostname` is the current hostname, `erl` is an Erlang version this package is compiled with
 and the `app` is an instance of `Package` class.  
Simple example:  

    {application, dummy, [
      {description, "Your description"},
      {vsn, {{ app.vsn}} },
      {registered, []},
      {applications, {{ app.std_apps + app.apps }} },
      {modules, {{ modules }}},
      {mod, {'super_service_app', []}},
      {env, []}
    ]}.
For package `dummy` it will produce following:  
    
    {application, dummy, [
          {description, "Your description"},
          {vsn, 1.0.0 },
          {registered, []},
          {applications, ['kernel', 'stdlib', 'dummy_dep'] },
          {modules, ['dummy_app']},
          {mod, {'super_service_app', []}},
          {env, []}
        ]}.
Env var example:

    {application, dummy, [
          {description, "Your description"},
          {vsn, {{ app.vsn}} },
          {registered, []},
          {applications, {{ app.std_apps + app.apps }} },
          {modules, {{ modules }}},
          {mod, {'super_service_app', []}},
          {env, [
                    {% if MAGIC_GREETING is defined %}
                      {greeting, "{{ MAGIC_GREETING }}"}
                    {% else %}
                      {greeting, "hello world!" }
                    {% endif %}
                  ]}
        ]}.
It will produce app file with `{greeting, "hello world"}` on machines without 
`MAGIC_GREETING` env var exported and will use its value when available.
# app.src advanced templates
To fill the power of templates you can use conditionals in app.src:

    {application, super_service, [
        {description, "Your description"},
        {vsn, {{ app.vsn}} },
        {registered, []},
        {applications, {{ app.std_apps + app.apps }} },
        {modules, {{ modules }}},
        {mod, {'super_service_app', []}},
        {env, [
            {static_var, “environment_independent_value”},
            {% set prod_hosts = ['super_prod1', 'super_prod2', 'super_prod3'] -%}
            {% set stage_hosts = ['super_stage1', 'super_stage2', 'super_stage3'] -%}
            {% if hostname in prod_hosts %}
                    {database, [
                            {host, "prod_db"},
                            {password, "super_secude_password"},
                            {pool_size, 400}
                    ]}
            {% elif hostname in stage_hosts %}
                    {database, [
                            {host, "staging_db"},
                            {password, "other_secude_password"},
                            {pool_size, 200}
                    ]}
            {% elif hostname == ‘bob_home_computer’ %}
                    {database, [
                            {host, "127.0.0.1"},
                            {password, "super_bob"},
                            {pool_size, 1}
                    ]}
            {% else %}
                    {database, [
                            {host, "dev_host"},
                            {password, "dev_password"},
                            {pool_size, 10}
                    ]}
            {% endif %},
            {% if app.git_branch == 'master' %}
                {log, info}
            {% else %}
                {log, debug}
            {% endif %}
        ]}
    ]}.
Here we used `hostname` to set proper database credentials for host and uses 
`Package.git_branch` property to force info loglevel on master branch. This
[Article](https://justtech.blog/2017/06/01/dynamic-configuration-erlang/) 
describes advantages of using conditional templates.