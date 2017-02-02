import json
from abc import ABC, abstractmethod
from os.path import join

from walrus.packages.dep import Dep
from walrus.utils.file_utils import write_file, read_erlang_file


def write_walrus_file(path, package_config):
    write_file(join(path, 'walrusfile.json'), package_config)


class ConfigFile(ABC):
    """Class for working with project configuration"""
    deps = []  # list(Dep) - deps from project configuration file
    name = ""
    path = ""
    drop_unknown = True
    with_source = True
    vsn = None
    compose_app_file = True
    app_deps = []  # list(string()) - deps from app.src or app file

    def __init__(self, path: str):
        print(path)
        res = read_erlang_file(join(path, 'src'), '.app.src')
        if not res:
            self.compose_app_file = False
            res = read_erlang_file(join(path, 'ebin'), '.app')
        if not res:
            raise ValueError('No app or app.src file in app!')
        self.set_app_primary_params(res)

    @abstractmethod
    def read_config(self):
        pass

    @abstractmethod
    def get_compiler(self):
        pass

    def need_walrusify(self):
        return True

    # Drop deps, which are not listed in app.src
    def drop_unknown_deps(self):
        for dep in self.deps:
            if dep.name not in self.app_deps and self.drop_unknown:
                print('Drop unused dep ' + dep.name)
                self.deps.remove(dep)

    def get_valrus_package(self):
        # TODO git repo
        return json.dumps(
            {'name': self.name,
             'with_source': self.with_source,
             'drop_unknown_deps': self.drop_unknown,
             'deps': Dep.export_all(self.deps),
             'vsn': self.vsn,
             'converted': True
             }, sort_keys=True, indent=4)

    def read_file(self, file):  # handle nofile
        with open(join(self.path, file), 'r') as f:
            return f.readlines()

    def set_app_primary_params(self, file):
        [decoded] = file
        (_, name, opts) = decoded
        self.name = name
        for (k, v) in opts:
            if k == 'vsn':
                self.vsn = v
            if k == 'applications':
                self.app_deps = v
