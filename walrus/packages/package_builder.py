from walrus.compiler.compiler_factory import get_compiler
from walrus.global_properties import WalrusGlobalProperties
from walrus.packages.config import config
from walrus.packages.config.config_factory import read_project
from walrus.packages.package import Package


class Builder:
    def __init__(self, path: str):
        super().__init__()
        self._system_config = WalrusGlobalProperties()
        self._path = path
        self._packages = {}
        self._project = Package.frompath(path)

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

    def walrusify(self):
        project_config = read_project(self.path)
        if project_config.need_walrusify():
            package_content = project_config.get_valrus_package()
            config.write_walrus_file(self.path, package_content)

    # Compose a package file
    def package(self):
        self.system_config.cache.package(self.project)

    # Parse package config, download missing deps to /tmp
    def populate(self):
        self.__populate_deps(self.project.deps)

    def build(self):
        self.__build_tree(self.project, is_subpackage=False)

    def deps(self):
        self.__build_deps(self.project, is_subpackage=False)

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
            self.system_config.cache.add_package(package)
        return res

    def __populate_deps(self, deps):  # TODO add an ability to operate with deps in parallel
        for name, dep in deps.items():
            if name not in self.packages:
                print('new dep: ' + name)
            if not self.system_config.cache.exists(dep):
                self.system_config.cache.fetch_package(dep)
            self.__populate_deps(dep.deps)
