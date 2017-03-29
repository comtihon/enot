import json

from walrus.packages.config import ConfigFile
from walrus.packages.config import config_factory
from walrus.packages.config.stub_config import StubConfig


class Package:
    def __init__(self, config=None, url=None, vsn=None):
        self._url = url
        self._vsn = vsn
        self._config = config
        self.__fill_deps()

    @property
    def url(self) -> str:  # git url
        return self._url

    @property
    def vsn(self) -> str:  # git tag / git commit hash
        return self._vsn

    @property
    def config(self) -> ConfigFile:  # ConfigFile
        return self._config

    @property
    def deps(self) -> dict:  # package's deps.
        return self._deps

    def fill_from_path(self, path):
        self._config = config_factory.upgrade_conf(path, self.config)
        self.__fill_deps()

    @classmethod
    def frompath(cls, path):
        config = config_factory.read_project(path)
        return cls(config=config)

    @classmethod
    def fromdeps(cls, name, dep):
        (url, vsn) = dep
        config = StubConfig(name)
        return cls(url=url, vsn=vsn, config=config)

    def export(self):
        return {'name': self.config.name,
                'url': self.url,
                'vsn': self.vsn,
                'deps': [dep.export() for _, dep in self.deps.items()]}

    def get_valrus_package(self):
        export = self.export()
        export_config = self.config.export()
        return json.dumps({**export, **export_config}, sort_keys=True, indent=4)

    # TODO is name enough unique?
    def get_name(self):
        return self.config.name

    def list_deps(self):
        return self.deps.values()

    def __fill_deps(self):
        self._deps = {}
        for name, dep in self.config.read_config().items():
            print(name + ' ' + str(dep))
            self.deps[name] = Package.fromdeps(name, dep)
