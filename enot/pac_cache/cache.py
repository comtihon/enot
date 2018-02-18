import tarfile
from abc import ABCMeta, abstractmethod
from enum import Enum
from os.path import join

from enot.pac_cache import Static
from enot.packages.package import Package
from enot.utils.file_utils import ensure_empty, copy_file
from enot.utils.logger import info


class CacheType(Enum):
    LOCAL = 'local'
    ENOT = 'enot'


class Cache(metaclass=ABCMeta):
    def __init__(self, name, temp_dir, path, default_erlang: str, cache_type: CacheType):
        self._erlang_version = Static.get_erlang_version(default_erlang)
        self._temp_dir = temp_dir
        self._path = path
        self._name = name
        self._cache_type = cache_type

    @property
    def cache_type(self) -> CacheType:
        return self._cache_type

    @property
    def temp_dir(self) -> str:
        return self._temp_dir

    @property
    def path(self) -> str:
        return self._path

    @property
    def name(self) -> str:
        return self._name

    @property
    def erlang_version(self) -> str:
        return self._erlang_version

    @abstractmethod
    def exists(self, package: Package) -> bool:
        pass

    @abstractmethod
    def fetch_package(self, package: Package):  # fetch package to system temp dir
        pass

    @abstractmethod
    def add_package(self, package: Package, rewrite=True) -> bool:  # add package to cache
        pass

    @abstractmethod
    def get_versions(self, fullname: str) -> list:  # get available package's versions
        pass

    @abstractmethod
    def get_erl_versions(self, fullname: str, version: str) -> list:  # get available package's erlang versions
        pass

    # Take ep package archived file from package, extract it to temp dir
    # and update package's path to point to extracted dir
    def unpackage(self, package: Package):  # TODO move me to package? use current dir + <something> instead of temp
        unpack_dir = join(self.temp_dir, package.fullname)
        enotpack = join(package.path, package.name + '.ep')
        ensure_empty(unpack_dir)
        info('Extract ' + enotpack)
        with tarfile.open(enotpack) as pack:
            pack.extractall(unpack_dir)
        package.path = unpack_dir  # update path pointer
        copy_file(enotpack, join(unpack_dir, package.name + '.ep'))

    def get_package_path(self, package: Package) -> str or None:
        return join(package.fullname, package.git_vsn, self.erlang_version)
