from walrus.pac_cache import CacheType
from walrus.pac_cache import cache_factory
from walrus.pac_cache.local_cache import LocalCache
from walrus.packages.package import Package


class CacheMan:
    local_cache: LocalCache = None
    caches = []

    def __init__(self, conf: dict):
        for cache in conf['cache']:
            cache_type = cache['type']
            cache = cache_factory.get_cache(cache_type, cache, conf['temp_dir'])
            if cache_type == CacheType.LOCAL.value:
                self.local_cache = cache
            else:
                self.caches.append(cache)

    # TODO add ability to customise search policy (build instead of fetching from remote etc...).
    def exists(self, package: Package):
        if self.local_cache.exists(package):  # local cache has this package
            return True
        for cache in self.caches:
            if cache.exists(package):  # remote cache has this package
                cache.fetch_package(package)
                self.local_cache.add_package(package)
                return True
        return False  # no cache has this package

    def link_package(self, package: Package, path: str):
        if self.local_cache:
            self.local_cache.link_package(package, path)

    def add_package(self, package: Package):
        if self.local_cache:
            self.local_cache.add_package(package)

    def fetch_package(self, package: Package):
        if self.local_cache:
            self.local_cache.fetch_package(package)
