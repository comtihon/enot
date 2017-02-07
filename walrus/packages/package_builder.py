from os.path import join

from walrus.compiler.compiler_factory import get_compiler
from walrus.global_properties import WalrusGlobalProperties
from walrus.pac_cache import cache_factory
from walrus.pac_cache.cache import Cache
from walrus.packages.config import config
from walrus.packages.config.config_factory import read_project
from walrus.packages.package import Package
from walrus.utils.file_utils import ensure_dir


class Builder:
    path = ""  # path in system of building project
    system_config = None  # system configuration
    packages = {}  # all project's packages

    def __init__(self, path: str):
        super().__init__()
        self.system_config = WalrusGlobalProperties()
        self.path = path

    def walrusify(self):
        project_config = read_project(self.path)
        if project_config.need_walrusify():
            package_content = project_config.get_valrus_package()
            config.write_walrus_file(self.path, package_content)

    def populate(self):
        first_level = Package.frompath(self.path)
        self.__populate_deps(first_level.deps)
        print(first_level.get_valrus_package())
        return True

    def __populate_deps(self, deps):  # TODO add an ability to operate with deps in parallel
        for name, dep in deps.items():
            if name not in self.packages:
                print('new dep: ' + name)
            if self.system_config.cache.exists(dep):
                self.system_config.cache.get_package(dep)
            else:
                self.system_config.cache.fetch_package(dep)
            self.__populate_deps(dep.deps)


# TODO build deps async
def build_package(path: str, walrus_global_config: WalrusGlobalProperties, package_config=None) -> bool:
    print('build ' + path)
    if not package_config:
        print('read project ' + path)
        package_config = read_project(path)
    package_cache = cache_factory.get_cache(walrus_global_config.temp_dir, walrus_global_config.cache_url)
    if package_config.deps is not []:
        ensure_dir(join(path, 'deps'))
    for dep in package_config.deps:
        if not package_cache.exists(dep):
            build_dep(package_cache, dep, walrus_global_config)
        package_cache.link_package(dep)
    print('compile ' + path)
    compiler = get_compiler(walrus_global_config, package_config)
    return compiler.compile()


def build_dep(package_cache: Cache, package: Package, walrus_global_config: WalrusGlobalProperties):
    print('build dep ' + package.get_name())
    path = package_cache.get_package(package, walrus_global_config)  # TODO refactor me
    if not build_package(join(walrus_global_config.temp_dir, package.get_name()), walrus_global_config):
        raise RuntimeError(package.get_name() + " can't be compiled")
    package_config = read_project(path)
    package_cache.add_package(package, path, package_config)  # TODO path should be in package
