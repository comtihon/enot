import json
import subprocess
import tarfile
from os.path import join
from subprocess import PIPE

import os
import coon
from jinja2 import Template
from pkg_resources import Requirement, resource_filename

from coon.packages.config import ConfigFile, CoonConfig
from coon.packages.config import config_factory
from coon.packages.config.stub_config import StubConfig
from coon.utils.file_utils import ensure_dir, write_file, read_file, copy_file


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
    def dep_packages(self) -> dict:  # package's deps.
        return self._deps

    @property
    def deps(self) -> list:  # package's deps names
        return list(self.dep_packages.keys())

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
    def from_deps(cls, name, dep, compiler=None):
        (url, vsn) = dep
        config = StubConfig(name, vsn, compiler)
        return cls(url=url, config=config)

    def export(self):
        return {'name': self.config.name,
                'url': self.url,
                'vsn': self.vsn,
                'deps': [dep.export() for _, dep in self.dep_packages.items()]}

    def to_package(self):
        export = self.export()
        export_config = self.config.export()
        return json.dumps({**export, **export_config}, sort_keys=True, indent=4)

    # Generate a release
    def to_release(self, relx_path: str) -> bool:
        ensure_dir(join(self.config.path, 'rel'))
        resave_relconf, relconf_path, relconf = self.__modify_resource('relx.config')
        resave_vmargs, vmargs_path, vmargs = self.__modify_resource('vm.args', 'rel')
        resave_sysconf, sysconf_path, sysconf = self.__modify_resource('sys.config', 'rel')
        try:
            p = subprocess.Popen(relx_path, stdout=PIPE, stderr=PIPE, cwd=self.config.path)
            if p.wait() != 0:
                print(self.name + ' release failed: ')
                print(p.stderr.read().decode('utf8'))
                print(p.stdout.read().decode('utf8'))
                return False
            else:
                return True
        finally:  # Return previous file values, if were changed.
            if resave_vmargs:
                write_file(vmargs_path, vmargs)
            if resave_relconf:
                write_file(relconf_path, relconf)
            if resave_sysconf:
                write_file(sysconf_path, sysconf)

    def list_deps(self) -> list():
        return self.dep_packages.values()

    def __fill_deps(self):
        self._deps = {}
        for name, dep in self.config.read_config().items():
            print(name + ' ' + str(dep))
            self.dep_packages[name] = Package.from_deps(name, dep)

    def __modify_resource(self, resource, path=''):
        resource_path = self.__ensure_resource(resource, path)
        resource = read_file(resource_path)
        if '{{ ' in resource:
            template = Template(resource)
            resource_filled = template.render(app=self)
            write_file(resource_path, resource_filled)
            return True, resource_path, resource
        return False, resource_path, resource

    def __ensure_resource(self, resource, path):
        resource_path = join(self.config.path, path, resource)
        if not os.path.isfile(resource_path):
            resource = resource_filename(Requirement.parse(coon.APPNAME), join('coon/resources', resource))
            print('copy ' + resource + ' to ' + resource_path)
            copy_file(resource, resource_path)
        return resource_path
