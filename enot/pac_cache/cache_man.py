from os.path import join

from enot.compiler.c_compiler import CCompiler
from enot.pac_cache import cache_factory
from enot.pac_cache.cache import CacheType, Cache
from enot.pac_cache.local_cache import LocalCache
from enot.pac_cache.remote_cache import RemoteCache
from enot.pac_cache.remote_cache_exception import RemoteCacheException
from enot.packages.package import Package
from enot.utils.logger import warning


class CacheMan:
    def __init__(self, conf: dict):
        self._local_cache = None
        self._caches = {}
        for cache in conf.get('cache', []):
            cache_type = CacheType(cache['type'])
            cache = cache_factory.get_cache(cache_type, cache, conf['temp_dir'], conf.get('default_erlang', '20'))
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
    def remote_caches(self) -> {str: RemoteCache}:
        return self._caches

    @property
    def official_cache(self):
        for cache in self.remote_caches.values():
            if cache.cache_type == CacheType.ENOT:
                return cache
        return None

    # Populate dep to become a package.
    # Try to find it in local cache, then in remote, finally fetch from git.
    def populate(self, dep: Package):
        if dep.url is not None and self.local_cache.exists(dep):  # local cache has this package
            path = join(self.local_cache.path, self.local_cache.get_package_path(dep))
            dep.update_from_cache(path)
            return
        for cache in self.remote_caches.values():
            if self.exists_remote(cache, dep):
                return
        self.local_cache.fetch_package(dep)

    # check if local cache contains this dep
    def exists_local(self, package: Package) -> bool:
        if package.url is not None and self.local_cache.exists(package):  # local cache has this package
            return True
        return False

    # check if local cache contains namespace/package_name/version
    def check_exists_local(self, fullname: str, vsn: str) -> bool:
        return self.local_cache.check_exists(join(fullname, vsn))

    def exists_remote(self, cache: Cache, dep: Package) -> bool:
        try:
            cache.fetch_package(dep)
            self.add_fetched(cache, dep)
            self.__fetch_all_deps(cache, dep)
            return True
        except RemoteCacheException as e:
            warning(cache.name + ': {0}'.format(e))
            return False
        except Exception as e:
            warning('Error from remote cache ' + cache.name + ': {0}'.format(e))
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

    def fetch_version(self, fullname: str, vsn: str) -> bool:
        for cache in self.remote_caches.values():
            maybe_package = cache.fetch_version(fullname, vsn)
            if maybe_package:
                self.add_fetched(cache, maybe_package)
                return True
        warning('No such package ' + fullname + ':' + vsn)
        return False

    def get_versions(self, fullname: str) -> list:
        for cache in self.remote_caches.values():
            versions = cache.get_versions(fullname)
            if versions:
                return versions
        warning('No such package ' + fullname)
        return []

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
