from abc import ABC, abstractmethod
from enum import Enum


class Compiler(Enum):
    WALRUS = 'walrus'
    ERLANG_MK = 'erlang.mk'
    REBAR = 'rebar'
    LOCAL = 'package-local'


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
