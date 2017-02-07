import json

from walrus.packages.config import config_factory


class Package:
    url = None  # git url
    vsn = None  # git tag / git commit hash
    config = None  # ConfigFile
    deps = {}  # list of deps.

    def __init__(self, config=None, url=None, vsn=None):
        self.url = url
        self.vsn = vsn
        self.config = config
        if config is not None:
            self.__fill_deps()

    def fill_from_path(self, path):
        config = config_factory.read_project(path)
        self.config = config
        self.__fill_deps()

    @classmethod
    def frompath(cls, path):
        config = config_factory.read_project(path)
        return cls(config=config)

    @classmethod
    def fromdeps(cls, dep):  # TODO should somehow form config here to know name of a dep at least
        (url, vsn) = dep
        return cls(url=url, vsn=vsn)

    def export(self):
        return {'name': self.config.name,
                'url': self.url,
                'vsn': self.vsn,
                'deps': [dep.export() for dep in self.deps]}

    def get_valrus_package(self):
        export = self.export()
        export_config = self.config.export()
        return json.dumps({**export, **export_config}, sort_keys=True, indent=4)

    # TODO is name enough unique?
    def get_name(self):
        return self.config.name

    def __fill_deps(self):
        deps = self.config.read_config()
        for name, dep in deps.items():
            self.deps[name] = Package.fromdeps(dep)
