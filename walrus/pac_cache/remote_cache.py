from walrus.packages.config import ConfigFile
from walrus.packages.walrus_package import WalrusPackage
from walrus.pac_cache import Cache
from walrus.global_properties import WalrusGlobalProperties


class RemoteCache(Cache):
    def __init__(self, protocol, path):
        self.path = path

    def exists(self, name):
        pass

    def link_package(self, name):
        # TODO download package to local cache
        pass

    def get_package(self, package: WalrusPackage, config: WalrusGlobalProperties):
        # TODO build package, load to repo
        pass

    def add_package(self, package: WalrusPackage, path, package_config: ConfigFile):
        # TODO unneeded
        pass
