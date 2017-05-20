from os.path import join

from erl_terms.erl_terms_core import decode

from coon.compiler.compiler_type import Compiler
from coon.packages.config.config import ConfigFile
from coon.utils.file_utils import read_file
from coon.packages.dep import Dep


class RebarConfig(ConfigFile):
    def __init__(self, path: str, vsn: str):
        super().__init__(vsn=vsn)
        self._path = path
        self._platform_defines = []
        rebarconfig = decode(read_file(join(path, 'rebar.config')))
        self.__parse_config(rebarconfig)

    @property
    def platform_defines(self) -> list:
        return self._platform_defines

    def get_compiler(self):
        return Compiler.REBAR

    def __parse_config(self, config):
        for (key, value) in config:
            if key == 'deps':
                self.__parse_deps(value)
            if key == 'erl_opts':
                self.__parse_erl_opts(value)

    def __parse_deps(self, deps):
        for (name, _, addr) in deps:
            (_, url, vsn) = addr
            (_, vsn_value) = vsn
            self.deps[name] = Dep(name, url, vsn_value)

    def __parse_erl_opts(self, value):
        for opt in value:
            if not isinstance(opt, str):
                if opt[0] == 'platform_define':
                    (_, case, define) = opt  # TODO come formats have 4-sized tuple
                    self.platform_defines.append({case, define})
                if opt[0] == 'd' and len(opt) == 2:  # opt[0] == d, opt[1] == {Var, Value}
                    self.build_vars.append(opt[1])
