import subprocess
from os.path import join
from subprocess import PIPE

import os
import coon
from pkg_resources import Requirement, resource_filename

from coon.compiler.abstract import AbstractCompiler
from coon.utils.file_utils import copy_file, ensure_dir


class CCompiler(AbstractCompiler):
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
        env_vars['BASEDIR'] = self.root_path
        env_vars['PROJECT'] = self.project_name
        env_vars['C_SRC_DIR'] = self.src_path
        env_vars['C_SRC_OUTPUT'] = join(self.output_path, self.project_name + '.so')
        for var in self.config.c_build_vars:
            for k, v in var.items():
                env_vars[k] = v
        p = subprocess.Popen(['make'], stdout=PIPE, stderr=PIPE, cwd=self.src_path, env=env_vars)
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
