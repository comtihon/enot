from abc import ABCMeta, abstractmethod


class Cachable(metaclass=ABCMeta):
    @abstractmethod
    @property
    def name(self):
        pass

    @abstractmethod
    @property
    def url(self):
        pass

    @abstractmethod
    @property
    def vsn(self):
        pass
