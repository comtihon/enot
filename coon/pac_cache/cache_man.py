from os.path import join

from coon.pac_cache import cache_factory

from coon.compiler.c_compiler import CCompiler
from coon.pac_cache.cache import CacheType, Cache
from coon.pac_cache.local_cache import LocalCache
from coon.packages.package import Package


class CacheMan:
    def __init__(self, conf: dict):
        self._local_cache = None
        self._caches = {}
        for cache in conf.get('cache', []):
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

    # TODO test me
    # TODO add ability to customise search policy (build instead of fetching from remote etc...).
    def exists(self, package: Package) -> bool:
        if package.url is not None and self.local_cache.exists(package):  # local cache has this package
            return True
        for cache in self.caches.values():
            if cache.exists(package) and cache.fetch_package(package):  # remote cache has this package
                return self.add_fetched(cache, package)  # TODO fetch all deps
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

    # Add package to remote cache. If recurse - add all package's deps to remote cache (if they are not there)
    # Package's deps should exist in local cache. It is guaranteed by calling `coon package`
    def add_package(self, package: Package, remote: str, rewrite: bool, recurse: bool) -> bool:
        if remote in self.caches.keys():
            if recurse:  # check deps in local cache
                self.__check_all_deps(package)
            result = self.caches[remote].add_package(package, rewrite)
            if recurse and result:
                self.__add_all_deps(self.caches[remote], package)  # TODO no package in local cache. Need to create it
            return result
        else:
            raise RuntimeError('Cache not found: ' + remote)

    # Untar package data, fill package conf, add to local cache
    def add_fetched(self, cache: Cache, package: Package):
        cache.unpackage(package)
        res = True
        if package.config.has_nifs:   # TODO test me
            res = CCompiler(package).compile()
        self.local_cache.add_package(package)
        return res

    # Fetch all deps (if they are not already fetched to local cache)
    def __fetch_all_deps(self, cache, package: Package):  # TODO where to use it?
        for name, dep in package.dep_packages.items():
            if not self.local_cache.exists(dep):
                if cache.exists(dep) and cache.fetch_package(dep):
                    self.add_fetched(cache, dep)  # TODO dep is now has full config, which is not needed
                    self.local_cache.add_package(package)
                    self.__fetch_all_deps(cache, dep)
                else:
                    raise RuntimeError('Dep not found ' + dep.name)

    # Check if all deps exist in local cache
    def __check_all_deps(self, package: Package):
        for name, dep in package.dep_packages.items():
            if not self.local_cache.exists(dep):
                raise RuntimeError('Dep ' + dep.name + ' not found in local cache. Rerun package.')
            self.__check_all_deps(dep)

    # Add all deps to cache
    def __add_all_deps(self, cache: Cache, package: Package):
        for name, dep in package.dep_packages.items():
            if not cache.exists(dep):
                # set dep's path - path to package in local cache
                dep.config.path = join(self.local_cache.path, self.local_cache.get_package_path(dep))
                cache.add_package(dep)
            self.__add_all_deps(cache, dep)
