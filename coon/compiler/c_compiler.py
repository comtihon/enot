import subprocess
from os.path import join
from subprocess import PIPE

import os
from pkg_resources import Requirement, resource_filename

import coon
from coon.compiler.abstract import AbstractCompiler
from coon.packages.package import Package
from coon.utils.file_utils import copy_file, ensure_dir


class CCompiler(AbstractCompiler):
    def __init__(self, package: Package, executable='make'):
        super().__init__(package, executable)

    @property
    def output_path(self) -> str:
        return join(self.config.path, 'priv')

    @property
    def src_path(self) -> str:
        return join(self.config.path, 'c_src')

    def compile(self) -> bool:
        ensure_dir(self.output_path)
        ensure_makefile(self.src_path)
        env_vars = dict(os.environ)
        for var in self.config.c_build_vars:
            for k, v in var.items():
                env_vars[k] = v
        p = subprocess.Popen([self.executable, '-C', 'c_src'],
                             stdout=PIPE,
                             stderr=PIPE,
                             cwd=self.config.path,
                             env=env_vars)
        if p.wait() != 0:
            print(self.project_name + ' compilation failed: ')
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            return True


# copy makefile to c_src from templates, if no makefile presents
def ensure_makefile(src_path):
    mkfile = join(src_path, 'Makefile')
    if not os.path.isfile(mkfile):
        resource = resource_filename(Requirement.parse(coon.APPNAME), 'coon/resources/CMakefile')
        print('copy ' + resource + ' to ' + mkfile)
        copy_file(resource, mkfile)
