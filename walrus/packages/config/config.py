from abc import ABC, abstractmethod
from os.path import join

from walrus.utils.file_utils import write_file, read_erlang_file


def write_walrus_file(path, package_config):
    write_file(join(path, 'walrusfile.json'), package_config)


class ConfigFile(ABC):
    """Class for working with project configuration"""

    def __init__(self, path: str):
        self._path = path
        self._prebuild = []
        self._build_vars = []
        self._has_nifs = False
        self._app_deps = []
        self._compose_app_file = True
        self._app_vsn = None
        self._conf_vsn = None
        self._with_source = True
        self._drop_unknown = True
        self._name = ""

    @property
    def name(self) -> str:  # project's name
        return self._name

    @property
    def path(self) -> str:  # project's path in filesystem
        return self._path

    @property
    def drop_unknown(self) -> bool:  # if deps, not specified in app.src should be dropped
        return self._drop_unknown

    @property
    def with_source(self) -> bool:  # include source, when adding to local cache
        return self._with_source

    @property
    def app_vsn(self) -> str:  # version from app.src.
        return self._app_vsn

    @property
    def conf_vsn(self) -> str or None:  # version from config
        return self._conf_vsn

    @property
    def vsn(self) -> str:   # conf version overrides app version
        if self.conf_vsn is None:
            return self.app_vsn
        return self.conf_vsn

    @property
    def compose_app_file(self) -> bool:  # should compose app file when compiling
        return self._compose_app_file

    @property
    def app_deps(self) -> list:  # deps from app.src or app file
        return self._app_deps

    @property
    def has_nifs(self) -> bool:  # git repo contains c_src folder
        return self._has_nifs

    @property
    def prebuild(self) -> list:  # actions to be run before the build
        return self._prebuild

    @property
    def build_vars(self) -> list:  # erlang build vars, passed to the compiler (string or tuple of size 2)
        return self._build_vars

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
                'drop_unknown_deps': self.drop_unknown,
                'has_nifs': self.has_nifs
                }

    def read_app_primary_params(self):
        res = read_erlang_file(join(self.path, 'src'), '.app.src')
        if not res:
            self._compose_app_file = False
            res = read_erlang_file(join(self.path, 'ebin'), '.app')
        if not res:  # TODO generate app file instead.
            raise ValueError('No app or app.src file in app!')
        self.__set_app_primary_params(res)

    def __set_app_primary_params(self, file):
        [decoded] = file
        (_, name, opts) = decoded
        self._name = name.replace("'", '')
        for (k, v) in opts:
            if k == 'vsn':
                self._app_vsn = v
            if k == 'applications':
                self._app_deps = v
