from walrus.pac_cache import Cache
from walrus.packages import Package


class RemoteCache(Cache):
    def __init__(self, protocol, path):
        super().__init__()
        self.path = path

    def exists(self, name):
        pass

    def fetch_package(self, package: Package):
        pass

    def link_package(self, name, path: str):
        # TODO download package to local cache
        pass

    def get_package(self, package: Package):
        # TODO build package, load to repo
        pass

    def add_package(self, package: Package, rewrite=False):
        # TODO unneeded
        pass
