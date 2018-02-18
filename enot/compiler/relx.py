import os
import socket
from os.path import join

from jinja2 import Template
from pkg_resources import Requirement, resource_filename

import enot
from enot.compiler.abstract import AbstractCompiler, run_cmd
from enot.pac_cache import Static
from enot.packages.config.config import ConfigFile
from enot.tool.relxtool import RelxTool
from enot.utils.file_utils import ensure_dir, write_file, read_file, copy_file, ensure_empty
from enot.utils.logger import debug


class RelxCompiler(AbstractCompiler):
    def __init__(self, package, executable='relx'):
        super().__init__(package, executable)
        self._tool = RelxTool()

    def compile(self,
                override_config: ConfigFile or None = None,
                params: list or None = None,
                erts: str or None = None) -> bool:
        ensure_dir(join(self.package.path, 'rel'))
        ensure_empty(join(self.package.path, '_rel'))
        resave_relconf, relconf_path, relconf = self.__modify_resource('relx.config')
        resave_vmargs, vmargs_path, vmargs = self.__modify_resource('vm.args', 'rel')
        resave_sysconf, sysconf_path, sysconf = self.__modify_resource('sys.config', 'rel')
        env_vars = dict(os.environ)
        if params is not None:
            cmd = [self.executable] + params
        else:
            cmd = self.executable
        if erts is not None:
            env_vars = RelxCompiler.__add_path(env_vars, erts)
        try:
            return run_cmd(cmd, self.project_name, self.root_path, env_vars=env_vars, output=None)
        finally:  # Return previous file values, if were changed.
            if resave_vmargs:
                write_file(vmargs_path, vmargs)
            if resave_relconf:
                write_file(relconf_path, relconf)
            if resave_sysconf:
                write_file(sysconf_path, sysconf)

    def __modify_resource(self, resource, path=''):
        resource_path = self.__ensure_resource(resource, path)
        resource = read_file(resource_path)
        if '{{ ' in resource:
            params = {x: os.environ[x] for x in os.environ}
            params['app'] = self.package
            params['hostname'] = socket.gethostname()
            params['erl'] = Static.get_erlang_version()
            resource_filled = Template(resource).render(params)
            write_file(resource_path, resource_filled)
            return True, resource_path, resource
        return False, resource_path, resource

    def __ensure_resource(self, resource, path):
        resource_path = join(self.package.path, path, resource)
        if not os.path.isfile(resource_path):
            resource = resource_filename(Requirement.parse(enot.APPNAME), join('enot/resources', resource))
            debug('copy ' + resource + ' to ' + resource_path)
            copy_file(resource, resource_path)
        return resource_path

    # params = ['-i', '<PATH>']
    @staticmethod
    def __add_path(env_vars, erts):
        path = env_vars['PATH']
        path += ':' + erts
        env_vars['PATH'] = path
        return env_vars
