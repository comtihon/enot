from abc import abstractmethod

from coon.pac_cache.cache import Cache
from coon.packages.package import Package


class RemoteCache(Cache):
    @abstractmethod
    def fetch_package(self, package: Package):
        pass

    @abstractmethod
    def exists(self, package: Package) -> bool:
        pass

    @abstractmethod
    def add_package(self, package: Package, rewrite=True) -> bool:
        pass

    @abstractmethod
    def fetch_version(self, fullname: str, version: str) -> Package or None:
        pass
