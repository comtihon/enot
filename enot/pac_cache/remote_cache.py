from abc import abstractmethod

from enot.pac_cache.cache import Cache
from enot.packages.package import Package


class RemoteCache(Cache):
    @abstractmethod
    def fetch_package(self, package: Package):
        pass

    def exists(self, package: Package) -> bool:
        return True

    @abstractmethod
    def add_package(self, package: Package, rewrite=True) -> bool:
        pass

    @abstractmethod
    def fetch_version(self, fullname: str, version: str) -> Package or None:
        pass
