from os.path import join

from erl_terms.erl_terms_core import decode

from coon.compiler.compiler_type import Compiler
from coon.packages.config.config import ConfigFile
from coon.utils.file_utils import read_file


class RebarConfig(ConfigFile):
    def __init__(self, path, has_nif):
        super().__init__(path)
        self._path = path
        self._has_nifs = has_nif
        self._platform_defines = []

    @property
    def platform_defines(self) -> list:
        return self._platform_defines

    def read_config(self):
        super().read_app_primary_params()
        rebarconfig = decode(read_file(join(self.path, 'rebar.config')))
        deps = {}
        for (key, value) in rebarconfig:
            if key == 'deps':
                deps = self.__parse_deps(value)
            if key == 'erl_opts':
                self.__parse_erl_opts(value)
        return deps

    def get_compiler(self):
        return Compiler.REBAR

    def __parse_deps(self, deps):
        return_deps = {}
        for (name, _, addr) in deps:
            if name in self.applications:
                (_, url, vsn) = addr
                (_, vsn_value) = vsn
                return_deps[name] = (url, vsn_value)
            else:
                print('Drop unused dep ' + name)
        return return_deps

    def __parse_erl_opts(self, value):
        for opt in value:
            if not isinstance(opt, str):
                if opt[0] == 'platform_define':
                    (_, case, define) = opt  # TODO come formats have 4-sized tuple
                    self.platform_defines.append({case, define})
                if opt[0] == 'd' and len(opt) == 2:  # opt[0] == d, opt[1] == {Var, Value}
                    self.build_vars.append(opt[1])
