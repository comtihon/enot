from abc import ABC, abstractmethod
from enum import Enum


class Compiler(Enum):
    WALRUS = 'walrus'
    ERLANG_MK = 'erlang.mk'
    REBAR = 'rebar'
    LOCAL = 'package-local'


class AbstractCompiler(ABC):
    project_name = ""
    compiler = "erlc"
    src_path = ""
    include_path = ""
    output_path = ""
    build_vars = []

    @abstractmethod
    def compile(self) -> bool:
        pass
