import json
import tarfile
from os.path import join

import os
from coon.packages.config import config_factory

from coon.packages.application_config import AppConfig
from coon.packages.config.config import ConfigFile
from coon.packages.config.coon import CoonConfig
from coon.packages.config.dep_config import DepConfig
from coon.utils.file_utils import tar


class Package:
    def __init__(self, path: str, config: ConfigFile or None, app_config: AppConfig or None, has_nifs=False):
        self._config = config
        self._app_config = app_config
        self._path = path
        self._has_nifs = has_nifs
        self.__set_deps()

    @property
    def name(self) -> str:
        if self.app_config:
            return self.app_config.name
        return self.config.name

    @property
    def url(self) -> str or None:  # git url
        return self.config.url

    @property
    def vsn(self) -> str:  # package dependency version
        if self.config.conf_vsn:  # TODO refactor versions!
            return self.config.conf_vsn
        return self.config.dep_vsn

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
    def deps(self) -> list:  # package's deps.
        return self._deps

    @property
    def std_apps(self) -> list:  # standard erlang apps. Used in app.src templates
        return ['kernel', 'stdlib']

    @property
    def apps(self) -> list:  # appliations, which should be run before this app
        apps = [dep.name for dep in self.deps]
        if self.app_config:
            apps += self.app_config.applications
        return list(set(apps))

    # package path contains c_src folder or priv/<project_name>.so (in case of local cache without c_src)
    @property
    def has_nifs(self) -> bool:
        return self._has_nifs

    @classmethod  # TODO url is not set here!
    def from_path(cls, path: str, url=None):
        config = config_factory.read_project(path, url=url)
        app_config = AppConfig.from_path(path)
        has_nifs = os.path.exists(join(path, 'c_src'))  # TODO search for .so files in priv?
        return cls(path, config, app_config, has_nifs)

    @classmethod  # TODO url is not set here!
    def from_package(cls, path: str, url=None):
        project_path, has_nifs, config, app_config = do_update_from_package(path, url)
        return cls(project_path, config, app_config, has_nifs)

    @classmethod
    def from_dep(cls, name, dep):
        (url, vsn) = dep
        return cls(None, DepConfig(name, vsn, url), None, False)

    # is called when dep is fetch by cache.
    def update_from_cache(self, path: str):
        self._config = config_factory.read_project(path, url=self.url, vsn=self.vsn)  # TODO unify versions!
        self._app_config = AppConfig.from_path(path)
        self._path = path
        self.__set_deps()

    def update_from_package(self, path: str, url):
        project_path, has_nifs, config, app_config = do_update_from_package(path, url)
        self.path = project_path
        self._config = config
        self._app_config = app_config
        self._has_nifs = has_nifs

    def export(self) -> dict:
        export = {'name': self.name,
                  'deps': [dep.export() for dep in self.deps]}
        if self.url is not None:
            export['url'] = self.url
        if self.vsn is not None:
            export['version'] = self.vsn
        return export

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

    def __set_deps(self):
        self._deps = []
        if self.config:
            for name, dep in self.config.deps.items():
                self._deps.append(Package.from_dep(name, dep))


def add_if_exist(src_dir, dir_to_add, dirs):
    if os.path.exists(join(src_dir, dir_to_add)):
        dirs.append(dir_to_add)


def do_update_from_package(path: str, url: str or None) -> (str, bool, ConfigFile, AppConfig or None):
    paths = path.split('/')
    package_name = paths[len(paths) - 1]
    project_path = '/'.join(paths[:-1])
    with tarfile.open(path) as pack:
        config = CoonConfig.from_package(pack, url)
        names = pack.getnames()
        conf_app_src = package_name + '.app.src'
        conf_app = package_name + '.app'
        if conf_app_src in names:
            app_config = AppConfig.from_package(conf_app_src, pack)
        elif conf_app in names:
            app_config = AppConfig.from_package(conf_app, pack, compose=False)
        else:
            app_config = None
        has_nifs = 'c_src' in names
    return project_path, has_nifs, config, app_config
