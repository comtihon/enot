from abc import ABC, abstractmethod
from enum import Enum


class Compiler(Enum):
    WALRUS = 'walrus'
    ERLANG_MK = 'erlang.mk'
    REBAR = 'rebar'
    LOCAL = 'package-local'


class AbstractCompiler(ABC):
    compiler = "erlc"
    src_path = ""
    include_path = ""
    output_path = ""

    @abstractmethod
    def compile(self):
        pass
