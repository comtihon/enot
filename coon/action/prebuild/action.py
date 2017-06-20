from abc import ABCMeta, abstractmethod
from enum import Enum


class ActionType(Enum):
    SHELL = 'shell'


class Action(metaclass=ABCMeta):
    @abstractmethod
    def run(self, path: str):
        pass

    @abstractmethod
    def export(self) -> dict:
        pass
