from os.path import join


from erl_terms.erl_terms_core import decode

from walrus.packages.config import ConfigFile
from walrus.packages.dep import Dep
from walrus.utils.file_utils import read_file
from walrus.compiler.abstract import Compiler


class RebarConfig(ConfigFile):
    def __init__(self, name, path, vsn):
        self.name = name
        self.path = path
        self.vsn = vsn

    def read_config(self):
        rebarconfig = decode(read_file(join(self.path, 'rebar.config')))
        for (key, value) in rebarconfig:
            if key == 'deps':
                self.parse_deps(value)
        self.drop_unknown_deps()

    # TODO handle non git deps, handle deps without (branch/tag, _)
    def parse_deps(self, deps):
        for (name, _, addr) in deps:
            (_, url, vsn) = addr
            self.deps.append(Dep(name, url, vsn))

    def get_compiler(self):
        return Compiler.REBAR
