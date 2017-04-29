from coon.compiler import CCompiler
from coon.pac_cache import CacheType, Cache, LocalCache
from coon.pac_cache import cache_factory
from coon.packages.package import Package


class CacheMan:
    def __init__(self, conf: dict):
        self._local_cache = None
        self._caches = {}
        for cache in conf['cache']:
            cache_type = CacheType(cache['type'])
            cache = cache_factory.get_cache(cache_type, cache, conf['temp_dir'])
            if cache_type == CacheType.LOCAL and isinstance(cache, LocalCache):
                if self._local_cache is not None:
                    raise RuntimeError('More that one local cache found in config!')
                self._local_cache = cache
            else:
                self.caches[cache.name] = cache

    @property
    def local_cache(self) -> LocalCache:
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
                return self.add_fetched(cache, package)
        return False  # no cache has this package

    def link_package(self, package: Package, dest_path: str):
        if self.local_cache:
            self.local_cache.link_package(package, dest_path)

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

    # Untar package data, fill package conf, add to local cache
    def add_fetched(self, cache, package):
        cache.unpackage(package)
        res = True
        if package.config.has_nifs:
            res = CCompiler(package.config).compile()
        self.local_cache.add_package(package)
        return res

    # Fetch all deps (if they are not already fetched to local cache)
    def fetch_all_deps(self, cache, package: Package):
        for name, dep in package.dep_packages.items():
            if not self.local_cache.exists(dep):
                if cache.exists(dep) and cache.fetch_package(dep):
                    self.add_fetched(cache, dep)  # TODO dep is now has full config, which is not needed
                    self.local_cache.add_package(package)
                    self.fetch_all_deps(cache, dep)
                else:
                    raise RuntimeError('Dep not found ' + dep.name)
