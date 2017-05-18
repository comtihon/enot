import json
import tarfile
from os.path import join

import os

from coon.packages.application_config import AppConfig
from coon.packages.cachable import Cachable
from coon.packages.config import config_factory
from coon.packages.config.config import ConfigFile
from coon.packages.config.coon import CoonConfig
from coon.utils.file_utils import tar


class Package(Cachable):
    def __init__(self, path: str, config: ConfigFile, app_config: AppConfig or None, url=None):
        self._url = url  # TODO should take url from local repo if None. (Should do it only if adding to local cache).
        self._config = config
        self._app_config = app_config
        self._path = path
        self._has_nifs = (os.path.exists(join(path, 'c_src')) or
                          os.path.isfile(join(path, 'priv', self.name + '.so')))

    @property
    def name(self) -> str:
        if self.app_config:
            return self.app_config.name
        return self.config.name

    @property  # TODO may be move url to config or app_config. In case of coon project?
    def url(self) -> str:  # git url
        return self._url

    @property
    def vsn(self) -> str:  # package version from configuration.
        return self.config.conf_vsn

    @property
    def path(self) -> str:  # packages path
        return self._path

    @path.setter
    def path(self, path):
        self._path = path

    @property
    def app_config(self) -> AppConfig or None:  # .app.src or .app
        return self._app_config

    @property
    def config(self) -> ConfigFile:  # ConfigFile
        return self._config

    @property
    def deps(self) -> dict:  # package's deps.
        return self.config.deps

    @property
    def std_apps(self) -> list:  # standard erlang apps. Used in app.src templates
        return ['kernel', 'stdlib']

    @property
    def apps(self) -> list:  # appliations, which should be run before this app
        apps = list(self.deps.keys())
        if self.app_config:
            apps += self.app_config.applications
        return list(set(apps))

    # package path contains c_src folder or priv/<project_name>.so (in case of local cache without c_src)
    @property
    def has_nifs(self) -> bool:
        return self._has_nifs

    @classmethod  # TODO url is not set here! (is set only when Dep -> Package during fetch)
    def from_path(cls, path: str):
        config = config_factory.read_project(path)
        app_config = AppConfig.from_path(path)
        return cls(path, config, app_config)

    # is called when dep is fetch by cache.
    @classmethod
    def from_cache(cls, path: str, dep: Cachable):
        config = config_factory.read_project(path, vsn=dep.vsn)
        app_config = AppConfig.from_path(path)
        return cls(path, config, app_config, url=dep.url)

    @classmethod  # TODO url is not set here!
    def from_package(cls, path: str, url=None):
        [package_name] = path.split('/')[-1:]
        with tarfile.open(path) as pack:
            config = CoonConfig.from_package(pack)
            names = pack.getnames()
            conf_app_src = package_name + '.app.src'
            conf_app = package_name + '.app'
            if conf_app_src in names:
                app_config = AppConfig.from_package(conf_app_src, pack)
            elif conf_app in names:
                app_config = AppConfig.from_package(conf_app, pack, compose=False)
            else:
                app_config = None
            # TODO get has_nifs from tar (because in constructor has nifs will be False)
        return cls(path, config, app_config, url=url)

    def export(self):
        return {'name': self.name,
                'url': self.url,
                'vsn': self.vsn,
                'deps': [dep.export() for _, dep in self.deps.items()]}

    def generate_package(self):
        pack_dir = self.path
        exported = self.export()
        config = join(pack_dir, 'coonfig.json')
        if not os.path.isfile(config):
            with open(join(pack_dir, 'coonfig.json'), 'w') as outfile:
                json.dump(exported, outfile, sort_keys=True, indent=4)
        dirs_to_add = []
        add_if_exist(pack_dir, 'ebin', dirs_to_add)
        add_if_exist(pack_dir, 'priv', dirs_to_add)
        if self.config.with_source:
            add_if_exist(pack_dir, 'src', dirs_to_add)
            add_if_exist(pack_dir, 'include', dirs_to_add)
            add_if_exist(pack_dir, 'c_src', dirs_to_add)
        add_if_exist(pack_dir, 'coonfig.json', dirs_to_add)
        package_dst = join(pack_dir, self.name + '.cp')
        print('create package ' + package_dst)
        tar(pack_dir, dirs_to_add, package_dst)

    def list_deps(self) -> list():
        return self.deps.values()


def add_if_exist(src_dir, dir_to_add, dirs):
    if os.path.exists(join(src_dir, dir_to_add)):
        dirs.append(dir_to_add)
