import json
from abc import ABC, abstractmethod
from os import listdir
from os.path import join

from erl_terms.erl_terms_core import decode

from walrus.packages.dep import Dep
from walrus.utils.file_utils import read_file, write_file


def write_walrus_file(path, package_config):
    write_file(join(path, 'walrusfile.json'), package_config)


def get_app_src(path):
    src = join(path, "src")
    [filesrc] = [f for f in listdir(src) if join(src, f).endswith(".app.src")]
    return read_file(join(src, filesrc))


class ConfigFile(ABC):
    """Class for working with project configuration"""
    deps = []
    name = ""
    path = ""
    drop_unknown = True
    with_source = True
    vsn = ()

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
        config_deps = self.get_deps_from_config()
        for dep in self.deps:
            if dep.name not in config_deps and self.drop_unknown:
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

    def get_deps_from_config(self):
        appconf = decode(get_app_src(self.path))
        [(_, _, opts)] = appconf
        for (opt, value) in opts:
            if opt == 'applications':
                return value
        print("no deps found")
        return []

    def read_file(self, file):  # handle nofile
        with open(join(self.path, file), 'r') as f:
            return f.readlines()
