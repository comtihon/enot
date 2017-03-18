from abc import ABC, abstractmethod
from enum import Enum


class ActionType(Enum):
    SHELL = 'shell'


class Action(ABC):
    @abstractmethod
    def run(self):
        pass
