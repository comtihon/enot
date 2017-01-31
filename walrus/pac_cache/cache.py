from abc import ABC, abstractmethod

from walrus.packages.config import ConfigFile
from walrus.packages.walrus_package import WalrusPackage
from walrus.global_properties import WalrusGlobalProperties


class Cache(ABC):
    path = ""

    @abstractmethod
    def exists(self, package: WalrusPackage):
        pass

    @abstractmethod
    def get_package(self, package: WalrusPackage, config: WalrusGlobalProperties):
        pass

    @abstractmethod
    def add_package(self, package: WalrusPackage, path: str, package_config: ConfigFile):
        pass

    def link_package(self, package: WalrusPackage):
        pass
