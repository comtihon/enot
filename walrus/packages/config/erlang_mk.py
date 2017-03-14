from os.path import join

from walrus.compiler.abstract import Compiler
from walrus.packages.config import ConfigFile
from walrus.utils.file_utils import read_file_lines


def get_deps_list(line):
    return line.split("=")[1].strip().split(" ")


def get_dep(line):
    [name, body] = line.split("=")
    [_, url, tag] = body.strip().split(" ")
    proper_name = name[4:]
    return proper_name.strip(), url, tag


class ErlangMkConfig(ConfigFile):
    def __init__(self, path, has_nif):
        super().__init__(path)
        self.path = path
        self.has_nifs = has_nif

    def read_config(self) -> dict:
        super().read_app_primary_params()
        content = read_file_lines(join(self.path, 'Makefile'))
        lines = [x.strip('\n') for x in content]
        return self.parse_deps(lines)

    def get_compiler(self):
        return Compiler.ERLANG_MK

    def parse_deps(self, lines):
        deps = []
        return_deps = {}
        for line in lines:
            if line.startswith('DEPS'):
                deps = get_deps_list(line)
            if line.startswith('dep_'):
                name, url, tag = get_dep(line)
                if name in deps and name in self.app_deps:
                    return_deps[name] = (url, tag)
                else:
                    print('Drop unused dep ' + name)
        return return_deps
