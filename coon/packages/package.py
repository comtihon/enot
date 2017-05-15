import json
import tarfile
from os.path import join

import os
from coon.packages.config import config_factory

from coon.packages.config.config import ConfigFile
from coon.packages.config.coon import CoonConfig
from coon.packages.config.stub_config import StubConfig
from coon.utils.file_utils import tar


class Package:
    def __init__(self, config=None, url=None):
        self._url = url
        self._config = config
        self.__fill_deps()

    @property
    def url(self) -> str:  # git url
        return self._url

    @property
    def vsn(self) -> str:  # package version from configuration.
        return self.config.vsn

    @property
    def config(self) -> ConfigFile:  # ConfigFile
        return self._config

    @property
    def deps(self) -> dict:  # package's deps.
        return self._deps

    @property
    def std_apps(self) -> list:  # standard erlang apps. Used in app.src templates
        return ['kernel', 'stdlib']

    @property
    def apps(self) -> list:  # appliations, which should be run before this app
        return list(set(self.config.applications + list(self.deps.keys())))

    # TODO is name enough unique?
    @property
    def name(self):
        return self.config.name

    def fill_from_path(self, path):
        self._config = config_factory.upgrade_conf(path, self.config)
        self.__fill_deps()

    @classmethod
    def from_path(cls, path: str):
        config = config_factory.read_project(path)
        return cls(config=config)

    @classmethod
    def from_package(cls, path: str):
        package_name = path.split('/')[-1:]
        with tarfile.open(path) as pack:
            config = CoonConfig(path)
            f = pack.extractfile(package_name)
            conf_json = f.read()
            config.init_from_json(conf_json)
        return cls(config=config)

    @classmethod
    def from_deps(cls, name, dep):
        (url, vsn) = dep
        config = StubConfig(name, vsn)
        return cls(url=url, config=config)

    def export(self):
        return {'name': self.config.name,
                'url': self.url,
                'vsn': self.vsn,
                'deps': [dep.export() for _, dep in self.deps.items()]}

    def generate_package(self):
        pack_dir = self.config.path
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

    def __fill_deps(self):
        self._deps = {}
        if self.config:
            for name, dep in self.config.read_config().items():
                print(name + ' ' + str(dep))
                self.deps[name] = Package.from_deps(name, dep)


def add_if_exist(src_dir, dir_to_add, dirs):
    if os.path.exists(join(src_dir, dir_to_add)):
        dirs.append(dir_to_add)
