import os
import shutil
from os.path import join

from walrus.packages.config import ConfigFile
from walrus.packages.walrus_package import WalrusPackage
from walrus.utils.file_utils import if_dir_exists, ensure_dir, link_if_needed
from walrus.pac_cache import Cache
from walrus.global_properties import WalrusGlobalProperties


def link(package_path, current_dir, name, dir_to_link):
    include_src = join(package_path, dir_to_link)
    include_dst = join(current_dir, 'deps', name, dir_to_link)
    link_if_needed(include_src, include_dst)


class LocalCache(Cache):
    def __init__(self, config):
        path = config.cache_url[7:]
        if not os.path.exists(path):
            os.makedirs(path)
        self.path = path

    def exists(self, package: WalrusPackage) -> bool:
        return if_dir_exists(self.path, package.get_package_path())

    # Fetch package to temp dir, return fetched path
    def get_package(self, package: WalrusPackage, config: WalrusGlobalProperties) -> str:
        return package.fetch_package(config.temp_dir)

    def add_package(self, package: WalrusPackage, path, package_config: ConfigFile):
        full_dir = join(self.path, package.get_package_path())
        ensure_dir(full_dir)
        print('copy ' + full_dir)
        shutil.copytree(join(path, 'ebin'), join(full_dir, 'ebin'))
        package_include = join(path, 'include')
        cache_include = join(full_dir, 'include')
        package_src = join(path, 'src')
        cache_src = join(full_dir, 'src')
        if os.path.exists(package_include):
            shutil.copytree(package_include, cache_include)
        if package_config.with_source:
            shutil.copytree(package_src, cache_src)

    def link_package(self, package: WalrusPackage):
        current_dir = os.getcwd()
        package_path = join(self.path, package.get_package_path())
        dep_dir = join(current_dir, 'deps', package.name)
        ensure_dir(dep_dir)
        print('link :' + package.name)
        link(package_path, current_dir, package.name, 'include')
        link(package_path, current_dir, package.name, 'src')
        link(package_path, current_dir, package.name, 'ebin')
