import subprocess
from os.path import join
from subprocess import PIPE

import os
import walrus
from jinja2 import Template
from pkg_resources import Requirement, resource_filename

from walrus.compiler.compiler_factory import get_compiler
from walrus.global_properties import WalrusGlobalProperties
from walrus.packages.package import Package
from walrus.utils.file_utils import copy_file, ensure_dir, read_file, write_file


class Builder:
    def __init__(self, path: str, package: Package):
        super().__init__()
        self._system_config = WalrusGlobalProperties()
        self._path = path
        self._packages = {}
        self._project = package

    @classmethod
    def init_from_path(cls, path):
        package = Package.frompath(path)
        return cls(path, package)

    @classmethod
    def init_from_package(cls, path_to_package):
        package = Package.frompackage(path_to_package)
        return cls(path_to_package, package)

    @property
    def path(self) -> str:  # path in system of building project
        return self._path

    @property
    def system_config(self) -> WalrusGlobalProperties:  # system configuration
        return self._system_config

    @property
    def packages(self) -> dict:  # all project's packages
        return self._packages

    @property
    def project(self) -> Package:  # root package being built
        return self._project

    # Compose a package file
    def package(self):
        self.system_config.cache.package(self.project)

    # Parse package config, download missing deps to /tmp
    def populate(self):
        self.__populate_deps(self.project.dep_packages.values())

    def add_package(self, remote, rewrite):
        self.system_config.cache.add_package(self.project, remote, rewrite)

    def build(self):
        self.__build_tree(self.project, is_subpackage=False)

    def deps(self):
        self.__build_deps(self.project, is_subpackage=False)

    # TODO check if there is a need to compile first.
    def release(self):
        ensure_dir(join(self.path, 'rel'))
        resave_relconf, relconf_path, relconf = self.__modify_resource('relx.config')
        resave_vmargs, vmargs_path, vmargs = self.__modify_resource('vm.args', 'rel')
        resave_sysconf, sysconf_path, sysconf = self.__modify_resource('sys.config', 'rel')
        try:  # TODO ensure relx installed. Install if not.
            p = subprocess.Popen('./relx', stdout=PIPE, stderr=PIPE, cwd=self.path)
            if p.wait() != 0:
                print(self.project.name + ' release failed: ')
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

    def __modify_resource(self, resource, path=''):
        resource_path = self.__ensure_resource(resource, path)
        resource = read_file(resource_path)
        if '{{ ' in resource:
            template = Template(resource)
            resource_filled = template.render(app=self.project)
            write_file(resource_path, resource_filled)
            return True, resource_path, resource
        return False, resource_path, resource

    def __ensure_resource(self, resource, path):
        resource_path = join(self.path, path, resource)
        if not os.path.isfile(resource_path):
            resource = resource_filename(Requirement.parse(walrus.APPNAME), join('walrus/resources', resource))
            print('copy ' + resource + ' to ' + resource_path)
            copy_file(resource, resource_path)
        return resource_path

    # TODO check all included in release apps for presence in deps
    # TODO take applications from app.src and add to relx.config
    def __check_app_deps(self, deps):
        # TODO ensure, if deps in relx.config are needed
        return

    # Build all deps, add to cache and link to project
    def __build_deps(self, package: Package, is_subpackage=True):
        # TODO check if package.config.path exists (if deps were populated before calling build_deps/build_tree)
        print('check ' + package.name)
        if is_subpackage and self.system_config.cache.exists(package):
            return True
        for dep in package.list_deps():
            if not self.system_config.cache.exists(dep):  # if package not in cache - build and add to cache
                if not self.__build_tree(dep):
                    raise RuntimeError('Can\'t built dep ' + dep.name)
            self.system_config.cache.link_package(dep, package.config.path)

    # Build package and it's deps
    def __build_tree(self, package: Package, is_subpackage=True):
        self.__build_deps(package, is_subpackage)  # TODO add an ability to compile deps in parallel
        compiler = get_compiler(self.system_config, package.config)
        res = compiler.compile()
        if is_subpackage and res:
            self.system_config.cache.add_package_local(package)
        return res

    # TODO lock deps after fetched.
    def __populate_deps(self, level):  # TODO add an ability to operate with deps in parallel
        next_level = []
        for dep in level:
            if dep.name not in self.packages:
                print('new dep: ' + dep.name)
                self.packages[dep.name] = dep.vsn
                if not self.system_config.cache.exists(dep):
                    self.system_config.cache.fetch_package(dep)
                next_level += dep.dep_packages.values()
            else:
                if dep.vsn != self.packages[dep.name]:  # Warn only if it is not the same dep
                    print('Skip ' + dep.name + ' (' + dep.vsn + '). Use ' + self.packages[dep.name])
        if next_level:
            self.__populate_deps(next_level)
