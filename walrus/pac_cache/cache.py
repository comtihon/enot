import shlex
from abc import ABC, abstractmethod

import subprocess

from walrus.packages import Package
from walrus.packages.config import ConfigFile


class Cache(ABC):
    path = ""
    erlang_version = None

    def __init__(self):
        self.erlang_version = Cache.get_erlang_version()

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
    def add_package(self, package: Package, path: str, package_config: ConfigFile):
        pass

    def link_package(self, package: Package):
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

