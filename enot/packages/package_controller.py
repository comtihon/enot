from os.path import join

from enot.packages.package_builder import Builder
from tinydb import TinyDB, where

from enot.global_properties import GlobalProperties
from enot.pac_cache.local_cache import LocalCache
from enot.packages.package import Package
from enot.utils.logger import warning, info

INSTALLED_TABLE = 'installed'
DATABASE_FILE = 'enot_index.json'


class Controller:
    def __init__(self) -> None:
        super().__init__()
        self._system_config = GlobalProperties()

    @property
    def system_config(self) -> GlobalProperties:  # system configuration
        return self._system_config

    @property
    def db_path(self) -> str:
        return join(self.system_config.conf_dir, DATABASE_FILE)

    @property
    def local_cache(self) -> LocalCache:
        return self.system_config.cache.local_cache

    def fetch(self, fullname: str, maybe_version: str or None) -> bool:
        vsn = self.fetch_package_version(fullname, maybe_version)
        return self.system_config.cache.fetch_version(fullname, vsn)

    def install(self, fullname: str, maybe_version: str or None) -> bool:
        vsn = self.get_package_version(fullname, maybe_version)
        if not self.system_config.cache.check_exists_local(fullname, vsn):
            if not self.fetch(fullname, vsn):
                warning('No ' + fullname + ':' + vsn + ' in local cache. Can\'t fetch it either.')
                info('Available versions for ' + fullname + ': ' + str(self.local_cache.get_versions(fullname)))
                return False
        erlang_vsns = self.local_cache.get_erl_versions(fullname, vsn)
        [latest_erl] = erlang_vsns[-1:]
        # populate and build deps
        builder = Builder.init_from_path(join(self.local_cache.path, fullname, vsn, latest_erl))
        builder.populate()
        builder.deps()
        if builder.project.install(self.system_config, latest_erl):
            self.__add_to_installed(fullname, vsn)
            info(fullname + ': ' + vsn + ' installed')
            return True
        return False

    def uninstall(self, fullname: str) -> bool:
        packages = self.__search_by_name(fullname)
        if not packages:
            warning(fullname + ' not installed')
            return False
        for package in packages:
            vsn = package['vsn']
            erlang_vsns = self.local_cache.get_erl_versions(fullname, package['vsn'])
            [latest_erl] = erlang_vsns[-1:]
            pack = Package.from_path(join(self.local_cache.path, fullname, vsn, latest_erl))
            if not pack.uninstall():
                warning('Can\'t uninstall package ' + fullname + ': ' + vsn)
                return False
            info(fullname + ': ' + vsn + ' uninstalled')
            self.__remove_from_installed(fullname, vsn)
        return True

    def installed(self) -> list:
        return self.__get_all_installed()

    # if version is none - search remote caches for versions
    def fetch_package_version(self, fullname: str, maybe_version: str or None) -> str:
        if maybe_version:
            return maybe_version
        versions = self.system_config.cache.get_versions(fullname)
        [last_vsn] = sorted(versions)[-1:]
        if not versions:
            raise RuntimeError('No versions for ' + fullname)
        return last_vsn

    # if version is none - search  remote
    def get_package_version(self, fullname: str, maybe_version: str or None) -> str:
        if maybe_version:
            return maybe_version
        return self.fetch_package_version(fullname, None)

    def __get_all_installed(self) -> list:
        db = TinyDB(self.db_path)
        try:
            table = db.table(INSTALLED_TABLE)
            return table.all()
        finally:
            db.close()

    def __add_to_installed(self, fullname: str, vsn: str):
        db = TinyDB(self.db_path)
        try:
            table = db.table(INSTALLED_TABLE)
            table.insert({'name': fullname, 'vsn': vsn})
        finally:
            db.close()

    def __search_by_name(self, fullname: str) -> list:
        db = TinyDB(self.db_path)
        try:
            table = db.table(INSTALLED_TABLE)
            packages = table.search(where('name') == fullname)
            return packages
        finally:
            db.close()

    def __remove_from_installed(self, fullname: str, vsn: str):
        db = TinyDB(self.db_path)
        try:
            table = db.table(INSTALLED_TABLE)
            table.remove((where('name') == fullname) & (where('vsn') == vsn))
        finally:
            db.close()
