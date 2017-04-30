import subprocess
from os.path import join
from subprocess import PIPE

import coon
import os
from coon.packages.package import Package
from jinja2 import Template
from pkg_resources import Requirement, resource_filename

from coon.compiler.abstract import AbstractCompiler
from coon.utils.file_utils import ensure_dir, write_file, read_file, copy_file


class RelxCompiler(AbstractCompiler):
    def __init__(self, package: Package, executable='./relx'):
        super().__init__(package, executable)

    def compile(self):
        ensure_dir(join(self.config.path, 'rel'))
        resave_relconf, relconf_path, relconf = self.__modify_resource('relx.config')
        resave_vmargs, vmargs_path, vmargs = self.__modify_resource('vm.args', 'rel')
        resave_sysconf, sysconf_path, sysconf = self.__modify_resource('sys.config', 'rel')
        try:
            p = subprocess.Popen(self.executable, stdout=PIPE, stderr=PIPE, cwd=self.config.path)
            if p.wait() != 0:
                print(self.package.name + ' release failed: ')
                print(p.stderr.read().decode('utf8'))
                print(p.stdout.read().decode('utf8'))
                return False
            else:
                return True
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
        resource_path = join(self.config.path, path, resource)
        if not os.path.isfile(resource_path):
            resource = resource_filename(Requirement.parse(coon.APPNAME), join('coon/resources', resource))
            print('copy ' + resource + ' to ' + resource_path)
            copy_file(resource, resource_path)
        return resource_path
