from os.path import join

from erl_terms.erl_terms_core import decode

from walrus.compiler.abstract import Compiler
from walrus.packages.config import ConfigFile
from walrus.utils.file_utils import read_file


class RebarConfig(ConfigFile):
    platform_defines = []

    def __init__(self, path, has_nif):
        super().__init__(path)
        self.path = path
        self.has_nifs = has_nif

    def read_config(self):
        super().read_app_primary_params()
        rebarconfig = decode(read_file(join(self.path, 'rebar.config')))
        deps = {}
        for (key, value) in rebarconfig:
            if key == 'deps':
                deps = self.parse_deps(value)
            if key == 'erl_opts':
                self.parse_erl_opts(value)
        return deps

    def parse_deps(self, deps):
        return_deps = {}
        for (name, _, addr) in deps:
            if name in self.app_deps:
                (_, url, vsn) = addr
                (_, vsn_value) = vsn
                return_deps[name] = (url, vsn_value)
            else:
                print('Drop unused dep ' + name)
        return return_deps

    def get_compiler(self):
        return Compiler.REBAR

    def parse_erl_opts(self, value):
        for opt in value:
            if not isinstance(opt, str):
                if opt[0] == 'platform_define':
                    (_, case, define) = opt  # TODO come formats have 4-sized tuple
                    self.platform_defines.append({case, define})
