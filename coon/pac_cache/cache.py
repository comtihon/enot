import os
import shlex
import subprocess
import tarfile
from abc import ABCMeta, abstractmethod
from enum import Enum
from os.path import join

from coon.packages.package import Package
from coon.utils.file_utils import ensure_empty, copy_file, read_file
from coon.utils.logger import info, critical


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

    # Take cp package archived file from package, extract it to temp dir
    # and update package's path to point to extracted dir
    def unpackage(self, package: Package):  # TODO move me to package? use current dir + <something> instead of temp
        unpack_dir = join(self.temp_dir, package.name)
        coonpack = join(package.path, package.name + '.cp')
        ensure_empty(join(self.temp_dir, package.name))
        info('Extract ' + coonpack)
        with tarfile.open(coonpack) as pack:
            pack.extractall(unpack_dir)
        package.path = unpack_dir  # update path pointer
        copy_file(coonpack, join(unpack_dir, package.name + '.cp'))

    def get_package_path(self, package: Package) -> str or None:
        return join(package.fullname, package.git_vsn, self.erlang_version)

    @staticmethod
    def get_erlang_version():
        vsn = Cache.find_erl_direct()
        if vsn is None:
            vsn = Cache.find_erl_version()
        if vsn is None:
            vsn = Cache.find_kerl_version()
        if vsn is None:
            critical('No Erlang found!')
            raise RuntimeError('No Erlang found :(')
        return vsn

    @staticmethod
    def find_erl_direct():
        try:
            vsn = subprocess.check_output(
                shlex.split("erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell"))
            return vsn.decode('utf-8').strip("\n\r\"")
        except subprocess.CalledProcessError:
            return None

    @staticmethod
    def find_erl_version(release='/usr/lib/erlang/releases/RELEASES'):
        try:
            file = read_file(release)
            return file.split(',')[2].strip('"')
        except FileNotFoundError:
            return None

    @staticmethod
    def find_kerl_version():
        kerl_path = os.environ.get('_KERL_ACTIVE_DIR')
        if kerl_path is not None:
            return Cache.find_erl_version(join(kerl_path, 'releases/RELEASES'))
        return None
