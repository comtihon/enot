import json
import shlex
import subprocess
import tarfile
from abc import ABC, abstractmethod
from enum import Enum
from os.path import join

import os

from walrus.packages import Package
from walrus.utils.file_utils import ensure_empty, copy_to, tar


class CacheType(Enum):
    LOCAL = 'local'
    ARTIFACTORY = 'artifactory'
    S3 = 's3'


class Cache(ABC):
    def __init__(self, temp_dir, path):
        self._erlang_version = Cache.get_erlang_version()
        self._temp_dir = temp_dir
        self._path = path

    @property
    def temp_dir(self) -> str:
        return self._temp_dir

    @property
    def path(self) -> str:
        return self._path

    @property
    def erlang_version(self) -> str:
        return self._erlang_version

    @abstractmethod
    def exists(self, package: Package):
        pass

    @abstractmethod
    def fetch_package(self, package: Package) -> bool:  # fetch package to system temp dir
        pass

    @abstractmethod
    def get_package(self, package: Package):
        pass

    @abstractmethod
    def add_package(self, package: Package, rewrite: bool):
        pass

    def link_package(self, package: Package, path: str):
        pass

    def package(self, package: Package):
        temp_dir = join(self.temp_dir, package.name)
        ensure_empty(temp_dir)
        exported = package.export()
        with open(join(temp_dir, package.name + '.json'), 'w') as outfile:
            json.dump(exported, outfile, sort_keys=True, indent=4)
        copy_to('ebin', temp_dir)
        if package.config.with_source:
            copy_to('src', temp_dir)
            copy_to('include', temp_dir)
            if package.config.has_nifs:
                copy_to('c_src', temp_dir)
        if package.config.has_nifs:
            copy_to('priv', temp_dir)
        tar(temp_dir, join(os.getcwd(), package.name + '.wp'))

    def unpackage(self, package: Package):
        pack_dir = join(self.temp_dir, package.name)
        walpack = pack_dir + '.wp'
        ensure_empty(join(self.temp_dir, package.name))
        with tarfile.open(walpack) as pack:
            pack.extractall(pack_dir)

    @staticmethod
    def get_erlang_version():
        proc = subprocess.run(
            shlex.split("erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell"),
            stdout=subprocess.PIPE)
        if proc.returncode == 0:
            version = proc.stdout.decode('utf-8').strip()
            return version.translate({ord(c): None for c in '"'})
        else:
            print("Can't get erlang version")  # TODO handle error
            return None
