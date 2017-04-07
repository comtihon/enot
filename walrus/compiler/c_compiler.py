import os
import subprocess
from os.path import join
from subprocess import PIPE

import walrus
from pkg_resources import Requirement, resource_filename

from walrus.compiler.abstract import AbstractCompiler
from walrus.packages.config import ConfigFile
from walrus.utils.file_utils import copy_file


# TODO call me before compiling with walrus compiler (c_src) and for all packages downloaded from remote cache with c_src
class CCompiler(AbstractCompiler):
    def __init__(self, config: ConfigFile):
        super().__init__()
        self._src_path = join(config.path, 'c_src')
        self._output_path = join(config.path, 'priv')
        self._project_name = config.name
        self._root_path = config.path

    def compile(self) -> bool:  # TODO can pass c compile options.
        resource = resource_filename(Requirement.parse(walrus.APPNAME), 'walrus/resources/CMakefile')
        print('copy ' + resource + ' to ' + join(self.src_path, 'Makefile'))
        copy_file(resource, join(self.src_path, 'Makefile'))
        env_vars = dict(os.environ)
        env_vars['BASEDIR'] = self.root_path
        env_vars['PROJECT'] = self.project_name
        env_vars['C_SRC_DIR'] = self.src_path
        env_vars['C_SRC_OUTPUT'] = self.output_path
        p = subprocess.Popen('make -C c_src', stdout=PIPE, stderr=PIPE, cwd=self.root_path, env=env_vars)
        if p.wait() != 0:
            print(self._project_name + ' compilation failed: ')
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            return True
