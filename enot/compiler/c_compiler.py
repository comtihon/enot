from os.path import join

from enot.packages.config.config import ConfigFile

import enot
import os
from pkg_resources import Requirement, resource_filename

from enot.compiler.abstract import AbstractCompiler, run_cmd
from enot.utils.file_utils import copy_file, ensure_dir
from enot.utils.logger import debug


class CCompiler(AbstractCompiler):
    def __init__(self, package, executable='make'):
        super().__init__(package, executable)

    @property
    def output_path(self) -> str:
        return join(self.package.path, 'priv')

    @property
    def src_path(self) -> str:
        return join(self.package.path, 'c_src')

    # TODO override unit to run cunit?

    def compile(self, override_config: ConfigFile or None = None) -> bool:
        ensure_dir(self.output_path)
        ensure_makefile(self.src_path)
        env_vars = self.__get_env_vars(override_config)
        return run_cmd([self.executable, '-C', 'c_src'],
                       self.project_name,
                       self.root_path,
                       env_vars)

    def __get_env_vars(self, override_config: ConfigFile or None) -> dict:
        env_vars = dict(os.environ)
        if override_config is not None and override_config.override_conf:
            vars_to_add = override_config.c_build_vars
        else:
            vars_to_add = self.package.config.c_build_vars
        for var in vars_to_add:
            for k, v in var.items():
                env_vars[k] = v
        return env_vars


# copy makefile to c_src from templates, if no makefile presents
def ensure_makefile(src_path):
    mkfile = join(src_path, 'Makefile')
    if not os.path.isfile(mkfile):
        resource = resource_filename(Requirement.parse(enot.APPNAME), 'enot/resources/CMakefile')
        debug('copy ' + resource + ' to ' + mkfile)
        copy_file(resource, mkfile)
