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
                self.remote_caches[cache.name] = cache

    # TODO may be move temp dir property here and send to caches on all operations needed?

    @property
    def local_cache(self) -> LocalCache:  # TODO multiple local caches?
        return self._local_cache

    @property
    def remote_caches(self) -> {str: Cache}:
        return self._caches

    # Populate dep to become a package.
    # Try to find it in local cache, then in remote, finally fetch from git.
    def populate(self, dep: Package):
        if dep.url is not None and self.local_cache.exists(dep):  # local cache has this package
            path = join(self.local_cache.path, self.local_cache.get_package_path(dep))
            dep.update_from_cache(path)
            return
        for cache in self.remote_caches.values():
            if cache.exists(dep):  # remote cache has this package
                package = cache.fetch_package(dep)  # dep -> package
                self.add_fetched(cache, package)
                return
        self.local_cache.fetch_package(dep)

    # check if local cache contains this dep
    def exists_local(self, package: Package) -> bool:
        if package.url is not None and self.local_cache.exists(package):  # local cache has this package
            return True
        return False

    # link package, return True if version changed (link updated)
    def link_package(self, package: Package, dest_path: str) -> bool:
        if self.local_cache:
            return self.local_cache.link_package(package, dest_path)

    def add_package_local(self, package: Package):
        if self.local_cache:
            self.local_cache.add_package(package)

    def fetch_package(self, package: Package):
        if self.local_cache:
            self.local_cache.fetch_package(package)

    # Add package to remote cache. If recurse - add all package's deps to remote cache (if they are not there)
    # Package's deps should exist in local cache. It is guaranteed by calling `coon package`
    def add_package(self, package: Package, remote: str, rewrite: bool, recurse: bool) -> bool:
        if remote in self.remote_caches.keys():
            if recurse:  # check deps in local cache
                self.__check_all_deps(package)
            result = self.remote_caches[remote].add_package(package, rewrite)
            if recurse and result:
                self.__add_all_deps(self.remote_caches[remote],
                                    package)  # TODO no package in local cache. Need to create it
            return result
        else:
            raise RuntimeError('Cache not found: ' + remote)

    # Untar package data, fill package conf, add to local cache
    def add_fetched(self, cache: Cache, package: Package):
        cache.unpackage(package)
        if package.has_nifs:  # TODO test me
            if not CCompiler(package).compile():
                raise RuntimeError(package.name + ' native compilation error.')
        self.local_cache.add_package(package)

    # Fetch all deps (if they are not already fetched to local cache)
    def __fetch_all_deps(self, cache, package: Package):  # TODO where to use it?
        for dep in package.deps:
            if not self.local_cache.exists(dep):
                if cache.exists(dep) and cache.fetch_package(dep):
                    self.add_fetched(cache, dep)
                    self.local_cache.add_package(package)
                    self.__fetch_all_deps(cache, dep)
                else:
                    raise RuntimeError('Dep not found ' + dep.name)

    # Check if all deps exist in local cache
    def __check_all_deps(self, package: Package):
        for dep in package.deps:
            if not self.local_cache.exists(dep):
                raise RuntimeError('Dep ' + dep.name + ' not found in local cache. Rerun package.')
            self.__check_all_deps(dep)

    # Add all deps to cache
    def __add_all_deps(self, cache: Cache, package: Package):
        for dep in package.deps:
            if not cache.exists(dep):
                # set dep's path - path to package in local cache
                dep.path = join(self.local_cache.path, self.local_cache.get_package_path(dep))
                cache.add_package(dep)
            self.__add_all_deps(cache, dep)
