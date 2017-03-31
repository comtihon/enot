from abc import ABC, abstractmethod
from enum import Enum


# TODO probably move me to separate file and make (string value, compiler constructor) and add get_compiler() method
class Compiler(Enum):
    WALRUS = 'walrus'  # prefer walrus
    ERLANG_MK = 'erlang.mk'  # prefer erlang.mk
    REBAR = 'rebar'  # use rebar everywhere
    NATIVE = 'native'   # use found by conf compiler (rebar.config, or erlang.mk exists)
    MAKEFILE = 'makefile'   # just call Makefile


class AbstractCompiler(ABC):
    def __init__(self):
        self._project_name = ""
        self._compiler = "erlc"
        self._root_path = ""
        self._src_path = ""
        self._include_path = ""
        self._output_path = ""
        self._build_vars = []

    @abstractmethod
    def compile(self) -> bool:
        pass

    @property
    def project_name(self) -> str:
        return self._project_name

    @property
    def compiler(self) -> str:
        return self._compiler

    @property
    def root_path(self) -> str:  # Project path.
        return self._root_path

    @property
    def src_path(self) -> str:
        return self._src_path

    @property
    def include_path(self) -> str:
        return self._include_path

    @property
    def output_path(self) -> str:
        return self._output_path

    @property
    def build_vars(self) -> list:
        return self._build_vars
