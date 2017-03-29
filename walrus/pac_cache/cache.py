import shlex
import subprocess
from abc import ABC, abstractmethod
from enum import Enum

from walrus.packages import Package


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
    def fetch_package(self, package: Package):
        pass

    @abstractmethod
    def get_package(self, package: Package):
        pass

    @abstractmethod
    def add_package(self, package: Package, rewrite: bool):
        pass

    def link_package(self, package: Package, path: str):
        pass

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
