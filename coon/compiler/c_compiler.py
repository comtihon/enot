from os.path import join

import coon
import os
from pkg_resources import Requirement, resource_filename

from coon.compiler.abstract import AbstractCompiler, run_cmd
from coon.packages.package import Package
from coon.utils.file_utils import copy_file, ensure_dir
from coon.utils.logger import debug


class CCompiler(AbstractCompiler):
    def __init__(self, package: Package, executable='make'):
        super().__init__(package, executable)

    @property
    def output_path(self) -> str:
        return join(self.package.path, 'priv')

    @property
    def src_path(self) -> str:
        return join(self.package.path, 'c_src')

    # TODO override unit to run cunit?

    def compile(self) -> bool:
        ensure_dir(self.output_path)
        ensure_makefile(self.src_path)
        env_vars = dict(os.environ)
        for var in self.package.config.c_build_vars:
            for k, v in var.items():
                env_vars[k] = v
        return run_cmd([self.executable, '-C', 'c_src'],
                       self.project_name,
                       self.root_path,
                       env_vars)


# copy makefile to c_src from templates, if no makefile presents
def ensure_makefile(src_path):
    mkfile = join(src_path, 'Makefile')
    if not os.path.isfile(mkfile):
        resource = resource_filename(Requirement.parse(coon.APPNAME), 'coon/resources/CMakefile')
        debug('copy ' + resource + ' to ' + mkfile)
        copy_file(resource, mkfile)
