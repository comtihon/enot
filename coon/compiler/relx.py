import os
from os.path import join

from jinja2 import Template
from pkg_resources import Requirement, resource_filename

import coon
from coon.compiler.abstract import AbstractCompiler, run_cmd
from coon.packages.config.config import ConfigFile
from coon.packages.package import Package
from coon.tool.relxtool import RelxTool
from coon.utils.file_utils import ensure_dir, write_file, read_file, copy_file, ensure_empty
from coon.utils.logger import debug


class RelxCompiler(AbstractCompiler):
    def __init__(self, package: Package, executable='relx'):
        super().__init__(package, executable)
        self._tool = RelxTool()

    def compile(self, override_config: ConfigFile or None = None):
        ensure_dir(join(self.package.path, 'rel'))
        ensure_empty(join(self.package.path, '_rel'))
        resave_relconf, relconf_path, relconf = self.__modify_resource('relx.config')
        resave_vmargs, vmargs_path, vmargs = self.__modify_resource('vm.args', 'rel')
        resave_sysconf, sysconf_path, sysconf = self.__modify_resource('sys.config', 'rel')
        try:
            return run_cmd(self.executable, self.project_name, self.root_path, output=None)
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
            resource_filled = Template(resource).render(app=self.package)
            write_file(resource_path, resource_filled)
            return True, resource_path, resource
        return False, resource_path, resource

    def __ensure_resource(self, resource, path):
        resource_path = join(self.package.path, path, resource)
        if not os.path.isfile(resource_path):
            resource = resource_filename(Requirement.parse(coon.APPNAME), join('coon/resources', resource))
            debug('copy ' + resource + ' to ' + resource_path)
            copy_file(resource, resource_path)
        return resource_path
