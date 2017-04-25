from abc import ABC, abstractmethod
from enum import Enum
from os.path import join

from walrus.packages.config import ConfigFile


# TODO probably move me to separate file and make (string value, compiler constructor) and add get_compiler() method
class Compiler(Enum):
    WALRUS = 'walrus'  # prefer walrus
    ERLANG_MK = 'erlang.mk'  # prefer erlang.mk
    REBAR = 'rebar'  # use rebar everywhere
    REBAR3 = 'rebar3'  # use rebar3 compiler
    NATIVE = 'native'  # use found by conf compiler (rebar.config, or erlang.mk exists)
    MAKEFILE = 'makefile'  # just call Makefile
    BOOTSTRAP = 'bootstrap'  # for those projects, who are afraid of Makefiles. Just call bootstrap in project's root


class AbstractCompiler(ABC):
    def __init__(self, config: ConfigFile, compiler='erlc'):
        self._config = config
        self._compiler = compiler

    @abstractmethod
    def compile(self) -> bool:
        pass

    @property
    def config(self) -> ConfigFile:
        return self._config

    @property
    def project_name(self) -> str:
        return self.config.name

    @property
    def compiler(self) -> str:
        return self._compiler

    @property
    def root_path(self) -> str:  # Project path.
        return self.config.path

    @property
    def src_path(self) -> str:
        return join(self.config.path, 'src')

    @property
    def include_path(self) -> str:
        return join(self.config.path, 'include')

    @property
    def output_path(self) -> str:
        return join(self.config.path, 'ebin')

    @property
    def build_vars(self) -> list:
        return self.config.build_vars
