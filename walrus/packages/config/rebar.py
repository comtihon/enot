from os.path import join


from erl_terms.erl_terms_core import decode

from walrus.packages.config import ConfigFile
from walrus.packages.dep import Dep
from walrus.utils.file_utils import read_file
from walrus.compiler.abstract import Compiler


class RebarConfig(ConfigFile):
    def __init__(self, path):
        super().__init__(path)
        self.path = path
        self.read_config()

    def read_config(self):
        rebarconfig = decode(read_file(join(self.path, 'rebar.config')))
        for (key, value) in rebarconfig:
            if key == 'deps':
                self.parse_deps(value)
        self.drop_unknown_deps()

    def parse_deps(self, deps):
        for (name, _, addr) in deps:
            (_, url, vsn) = addr
            (_, vsn_value) = vsn
            self.deps.append(Dep(name, url, vsn_value))

    def get_compiler(self):
        return Compiler.REBAR
