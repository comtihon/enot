import json
import tarfile

from coon.packages.config import config_factory

from coon.packages.config.config import ConfigFile
from coon.packages.config.coon import CoonConfig
from coon.packages.config.stub_config import StubConfig


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

    def to_package(self):
        export = self.export()
        export.update(self.config.export())
        return json.dumps(export, sort_keys=True, indent=4)

    def list_deps(self) -> list():
        return self.deps.values()

    def __fill_deps(self):
        self._deps = {}
        if self.config:
            for name, dep in self.config.read_config().items():
                print(name + ' ' + str(dep))
                self.deps[name] = Package.from_deps(name, dep)
