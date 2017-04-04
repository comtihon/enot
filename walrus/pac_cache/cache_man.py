from walrus.pac_cache import CacheType, Cache
from walrus.pac_cache import cache_factory
from walrus.packages.package import Package


class CacheMan:
    def __init__(self, conf: dict):
        for cache in conf['cache']:
            cache_type = cache['type']
            cache = cache_factory.get_cache(cache_type, cache, conf['temp_dir'])
            self._local_cache = None
            self._caches = {}
            if cache_type == CacheType.LOCAL.value:
                self._local_cache = cache
            else:
                self.caches[cache.name] = cache

    @property
    def local_cache(self) -> Cache:
        return self._local_cache

    @property
    def caches(self) -> {str: Cache}:
        return self._caches

    # TODO add ability to customise search policy (build instead of fetching from remote etc...).
    def exists(self, package: Package) -> bool:
        if self.local_cache.exists(package):  # local cache has this package
            return True
        for cache in self.caches.values():
            if cache.exists(package) and cache.fetch_package(package):  # remote cache has this package
                cache.unpackage(package)
                self.local_cache.add_package(package)
                return True
        return False  # no cache has this package

    def link_package(self, package: Package, path: str):
        if self.local_cache:
            self.local_cache.link_package(package, path)

    def add_package_local(self, package: Package):
        if self.local_cache:
            self.local_cache.add_package(package)

    def fetch_package(self, package: Package):
        if self.local_cache:
            self.local_cache.fetch_package(package)

    def package(self, package: Package):
        if self.local_cache:
            self.local_cache.package(package)

    def add_package(self, package: Package, remote: str, rewrite: bool):
        if remote in self.caches.keys():
            return self.caches[remote].add_package(package, rewrite)
        else:
            raise RuntimeError('Cache not found: ' + remote)
