from os.path import join
from walrus.packages.config import ConfigFile
from walrus.packages.dep import Dep
from walrus.utils.file_utils import read_file_lines
from walrus.compiler.abstract import Compiler


def get_deps_list(line):
    return line.split("=")[1].strip().split(" ")


def get_dep(line):
    [name, body] = line.split("=")
    [_, url, tag] = body.strip().split(" ")
    proper_name = name[4:]
    return proper_name.strip(), url, tag


class ErlangMkConfig(ConfigFile):
    def __init__(self, path):
        super().__init__(path)
        self.path = path
        self.read_config()

    def read_config(self):
        content = read_file_lines(join(self.path, 'Makefile'))
        lines = [x.strip('\n') for x in content]

        deps = []
        for line in lines:
            if line.startswith('DEPS'):
                deps = get_deps_list(line)
            if line.startswith('dep_'):
                name, url, tag = get_dep(line)
                if name in deps:
                    self.deps.append(Dep(name, url, tag))
                else:
                    print('Skip unknown dep ' + name)
        self.drop_unknown_deps()

    def get_compiler(self):
        return Compiler.ERLANG_MK
