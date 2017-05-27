from abc import ABCMeta, abstractmethod
from os.path import join

from coon.compiler.compiler_type import Compiler
from coon.utils.file_utils import write_file

"""
Project config file. Can be rebar.config (Rebar1-3), config in Makefile (erlang.mk) or config.json (coon)
"""


def write_coonfig(path, package_config):
    write_file(join(path, 'coonfig.json'), package_config)


class ConfigFile(metaclass=ABCMeta):
    def __init__(self):
        self._prebuild = []
        self._build_vars = []
        self._c_build_vars = []
        self._deps = {}
        self._with_source = True
        self._drop_unknown = True
        self._name = ''
        self._conf_vsn = None
        self._git_vsn = None
        self._url = None

    @property
    def name(self) -> str:  # project's name
        return self._name

    @name.setter
    def name(self, name):
        self._name = name

    @property
    def drop_unknown(self) -> bool:  # if deps, not specified in app.src should be dropped
        return self._drop_unknown

    @property
    def with_source(self) -> bool:  # include source, when adding to local cache
        return self._with_source

    @property
    def conf_vsn(self) -> str or None:  # version from config  # TODO move me to application config?
        return self._conf_vsn

    @property
    def git_vsn(self) -> str or None:  # version from git (can be None in most cases)
        return self._git_vsn

    @git_vsn.setter
    def git_vsn(self, vsn):
        self._git_vsn = vsn

    @property
    def deps(self) -> dict:  # deps from config file. Dict of (url, vsn), where keys are their names
        return self._deps

    @property
    def prebuild(self) -> list:  # actions to be run before the build
        return self._prebuild

    @property
    def build_vars(self) -> list:  # erlang build vars, passed to the compiler (string or tuple of size 2)
        return self._build_vars

    @property
    def c_build_vars(self) -> list:  # c build vars. list of kv tuples
        return self._c_build_vars

    @property
    def url(self) -> str:
        return self._url

    @url.setter
    def url(self, url):
        self._url = url

    @abstractmethod
    def get_compiler(self) -> Compiler:
        pass

    def need_coonsify(self):
        return True

    def set_url(self, url: str):
        self._url = url

    def export(self) -> dict:
        return {'with_source': self.with_source,
                'drop_unknown_deps': self.drop_unknown,
                'build_vars': self.build_vars,  # TODO skip if empty
                'c_build_vars': self.c_build_vars,
                'prebuild': self.prebuild
                }
