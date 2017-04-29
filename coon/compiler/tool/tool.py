from abc import ABC, abstractmethod

from coon.compiler import Compiler


class AbstractTool(ABC):
    @abstractmethod
    @property
    def compiler(self) -> Compiler:
        pass

    @abstractmethod
    @property
    def name(self) -> str:
        pass

    @abstractmethod
    @property
    def url(self) -> str:
        pass

    @abstractmethod
    @property
    def version(self) -> str:
        pass

    @abstractmethod
    def build(self, path: str) -> str:
        pass
