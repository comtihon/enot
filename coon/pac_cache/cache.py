import shlex
import subprocess
import tarfile
from abc import ABCMeta, abstractmethod
from os.path import join

from enum import Enum

from coon.packages.cachable import Cachable
from coon.utils.file_utils import ensure_empty, copy_file
from coon.packages.package import Package


class CacheType(Enum):
    LOCAL = 'local'
    ARTIFACTORY = 'artifactory'
    S3 = 's3'


class Cache(metaclass=ABCMeta):
    def __init__(self, name, temp_dir, path):
        self._erlang_version = Cache.get_erlang_version()
        self._temp_dir = temp_dir
        self._path = path
        self._name = name

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
    def exists(self, package: Cachable) -> bool:
        pass

    @abstractmethod
    def fetch_package(self, package: Cachable) -> Package:  # fetch package to system temp dir
        pass

    @abstractmethod
    def add_package(self, package: Package, rewrite=True) -> bool:  # add package to cache
        pass

    def unpackage(self, package: Cachable) -> Package:
        pack_dir = join(self.temp_dir, package.name)
        coonpack = pack_dir + '.cp'
        ensure_empty(join(self.temp_dir, package.name))
        with tarfile.open(coonpack) as pack:
            pack.extractall(pack_dir)
        copy_file(coonpack, join(pack_dir, package.name + '.cp'))
        return Package.from_cache(pack_dir, package)

    def get_package_path(self, package: Cachable):
        namespace = package.url.split('/')[-2]
        return join(namespace, package.name, package.vsn, self.erlang_version)

    @staticmethod
    def get_erlang_version():
        vsn = subprocess.check_output(  # TODO handle error
            shlex.split("erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell"))
        return vsn.decode('utf-8').strip("\n\r\"")
