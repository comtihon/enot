from os.path import join

from coon.pac_cache import cache_factory

from coon.compiler.c_compiler import CCompiler
from coon.pac_cache.cache import CacheType, Cache
from coon.pac_cache.local_cache import LocalCache
from coon.packages.package import Package
from coon.utils.logger import warning


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
            try:
                if self.exists_remote(cache, dep):
                    return
            except RuntimeError:
                continue
        self.local_cache.fetch_package(dep)

    # check if local cache contains this dep
    def exists_local(self, package: Package) -> bool:
        if package.url is not None and self.local_cache.exists(package):  # local cache has this package
            return True
        return False

    def exists_remote(self, cache: Cache, dep: Package) -> bool:
        try:
            if cache.exists(dep):  # remote cache has this package
                cache.fetch_package(dep)
                self.add_fetched(cache, dep)
                self.__fetch_all_deps(cache, dep)
                return True
        except Exception as e:
            warning('Error from remote cache ' + cache.name + ': {0}'.format(e))
            raise RuntimeError(e)

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
            try:
                return self.__add_with_deps(self.remote_caches[remote],
                                            package,
                                            rewrite,
                                            recurse)  # TODO no package in local cache. Need to create it
            except RuntimeError:
                return False
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
    def __fetch_all_deps(self, cache: Cache, package: Package):
        for dep in package.deps:
            if not self.local_cache.exists(dep):
                if cache.exists(dep):
                    cache.fetch_package(dep)
                    self.add_fetched(cache, dep)
                    self.__fetch_all_deps(cache, dep)
                else:
                    warning('Dep ' + dep.name + ' not found in ' + cache.name)
                    self.__obtain_missing_dep(cache, dep)
                    self.__fetch_all_deps(cache, dep)

    # search for missing dep in other remote caches. If nothing found - fetch, build and add it manually
    def __obtain_missing_dep(self, not_found_cache: Cache, dep: Package):
        other_remote = self.remote_caches
        for cache in other_remote.values():  # try to find dep in other remotes
            if cache is not not_found_cache:
                if cache.exists(dep):
                    warning('Took dep ' + dep.name + ' from ' + cache.name)
                    cache.fetch_package(dep)
                    self.add_fetched(cache, dep)
                    return True
        warning('Should fetch and build missing dep ' + dep.name)
        self.local_cache.fetch_package(dep)
        return self.local_cache.add_package(dep)

    # Check if all deps exist in local cache
    def __check_all_deps(self, package: Package):
        for dep in package.deps:
            if not self.local_cache.exists(dep):
                raise RuntimeError('Dep ' + dep.name + ' not found in local cache. Rerun package.')
            self.__check_all_deps(dep)

    # Add package to cache. If recurse - add all it's deps also
    def __add_with_deps(self, cache: Cache, package: Package, rewrite: bool, recurse: bool) -> bool:
        try:
            result = cache.add_package(package, rewrite)
        except Exception as e:
            warning('Error from remote cache ' + cache.name + ': {0}'.format(e))
            raise RuntimeError
        if result and recurse:
            for dep in package.deps:
                if rewrite or not cache.exists(dep):
                    # set dep's path - path to package in local cache
                    dep.path = join(self.local_cache.path, self.local_cache.get_package_path(dep))
                    self.__add_with_deps(cache, dep, rewrite, recurse)
        return result
