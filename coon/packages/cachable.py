from abc import ABCMeta, abstractmethod


class Cachable(metaclass=ABCMeta):
    @property
    @abstractmethod
    def name(self):
        pass

    @property
    @abstractmethod
    def url(self):
        pass

    @property
    @abstractmethod
    def vsn(self):
        pass
