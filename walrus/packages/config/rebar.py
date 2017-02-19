from os.path import join

from erl_terms.erl_terms_core import decode

from walrus.compiler.abstract import Compiler
from walrus.packages.config import ConfigFile
from walrus.utils.file_utils import read_file


class RebarConfig(ConfigFile):
    def __init__(self, path):
        super().__init__(path)
        self.path = path

    def read_config(self):
        super().read_app_primary_params()
        rebarconfig = decode(read_file(join(self.path, 'rebar.config')))
        for (key, value) in rebarconfig:
            if key == 'deps':
                return self.parse_deps(value)
        return {}

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
