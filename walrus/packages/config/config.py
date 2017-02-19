from abc import ABC, abstractmethod
from os.path import join

from walrus.utils.file_utils import write_file, read_erlang_file


def write_walrus_file(path, package_config):
    write_file(join(path, 'walrusfile.json'), package_config)


class ConfigFile(ABC):
    """Class for working with project configuration"""
    name = ""  # project's name
    path = ""  # project's path in filesystem
    drop_unknown = True  # if deps, not specified in app.src should be dropped
    with_source = True  # include source, when adding to local cache
    app_vsn = None  # version from app.src
    compose_app_file = True  # should compose app file when compiling
    app_deps = []  # deps from app.src or app file

    def __init__(self, path: str):
        self.path = path

    # read config, return deps
    @abstractmethod
    def read_config(self) -> dict:
        pass

    @abstractmethod
    def get_compiler(self):
        pass

    def need_walrusify(self):
        return True

    def export(self):
        return {'name': self.name,
                'with_source': self.with_source,
                'drop_unknown_deps': self.drop_unknown
                }

    def read_app_primary_params(self):
        res = read_erlang_file(join(self.path, 'src'), '.app.src')
        if not res:
            self.compose_app_file = False
            res = read_erlang_file(join(self.path, 'ebin'), '.app')
        if not res:
            raise ValueError('No app or app.src file in app!')
        self.__set_app_primary_params(res)

    def __set_app_primary_params(self, file):
        [decoded] = file
        (_, name, opts) = decoded
        self.name = name.replace("'", '')
        for (k, v) in opts:
            if k == 'vsn':
                self.app_vsn = v
            if k == 'applications':
                self.app_deps = v
