from abc import ABCMeta, abstractmethod
from enum import Enum


class ActionType(Enum):
    SHELL = 'shell'
    RELEASE = 'release'


class Action(metaclass=ABCMeta):
    @abstractmethod
    def run(self, path: str, config=None, system_config=None, erlang_vsn: str = None) -> bool:
        pass

    @abstractmethod
    def export(self) -> dict:
        pass
