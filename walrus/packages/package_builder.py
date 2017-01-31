from os.path import join

from walrus.packages.config import ConfigFile
from walrus.packages.config import config
from walrus.packages.config.config_factory import read_project
from walrus.packages.walrus_package import WalrusPackage
from walrus.utils.file_utils import ensure_dir
from walrus.compiler.compiler_factory import get_compiler
from walrus.pac_cache import cache_factory
from walrus.pac_cache import Cache
from walrus.global_properties import WalrusGlobalProperties


def walrusify(path, write_file=True):   # TODO no need to write files in tmp projects
    project_config = read_project(path)
    if project_config.need_walrusify():
        package_content = project_config.get_valrus_package()
        config.write_walrus_file(path, package_content)  # TODO return WalrusConfig
    return True


# TODO build deps async
def build_package(path: str, walrus_global_config: WalrusGlobalProperties) -> bool:
    package_config = read_project(path)
    package_cache = cache_factory.get_cache(walrus_global_config)
    if package_config.deps is not []:
        ensure_dir(join(path, 'deps'))
    for dep in package_config.deps:
        package = WalrusPackage(dep, walrus_global_config)
        if not package_cache.exists(package):
            build_dep(package_cache, package, package_config, walrus_global_config)
        package_cache.link_package(package)
    print('compile ' + path)
    compiler = get_compiler(walrus_global_config, package_config)
    return compiler.compile()


def build_dep(package_cache: Cache, package: WalrusPackage, package_config: ConfigFile,
              walrus_global_config: WalrusGlobalProperties):
    path = package_cache.get_package(package, walrus_global_config)  # TODO refactor me
    walrusify(path, write_file=False)  # TODO refactor me
    if not build_package(join(walrus_global_config.temp_dir, package.name), walrus_global_config):
        raise RuntimeError(package.name + " can't be compiled")
    package_cache.add_package(package, path, package_config)  # TODO path should be in package
