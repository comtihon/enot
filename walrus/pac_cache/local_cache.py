import os
import shutil
from os.path import join

from git import Repo

from walrus.pac_cache import Cache
from walrus.packages.config import ConfigFile
from walrus.packages.package import Package
from walrus.utils.file_utils import if_dir_exists, ensure_dir, link_if_needed
from walrus.utils.file_utils import remove_dir


class LocalCache(Cache):
    temp_dir = ""

    def __init__(self, temp_dir, cache_url):
        super().__init__()
        path = cache_url[7:]
        if not os.path.exists(path):
            os.makedirs(path)
        self.path = path
        self.temp_dir = temp_dir

    def exists(self, package: Package) -> bool:  # TODO error here. If package non existent - no config of package
        return if_dir_exists(self.path, self.__get_package_path(package))

    # clone git repo to tmp, make package to scan and update it's config
    def fetch_package(self, package: Package):  # TODO error here. If package non existent - no config of package
        temp_path = join(self.temp_dir, package.get_name())
        print('fetch ' + temp_path)
        remove_dir(self.temp_dir)
        repo = Repo.clone_from(package.url, temp_path)
        assert not repo.bare
        tag = repo.create_head(package.vsn)
        repo.head.reference = tag
        repo.head.reset(index=True, working_tree=True)
        LocalCache.fill_package_from_temp(package, temp_path)

    # make package to scan and update it's config
    def get_package(self, package: Package):  # TODO error here. If package non existent - no config of package
        LocalCache.fill_package_from_temp(package, join(self.temp_dir, package.get_name()))

    # add built package to local cache
    def add_package(self, package: Package, path, package_config: ConfigFile):
        full_dir = join(self.path, self.__get_package_path(package))
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

    # link package from local cache to project
    def link_package(self, package: Package):
        current_dir = os.getcwd()
        package_path = join(self.path, self.__get_package_path(package))
        package_name = package.get_name()
        dep_dir = join(current_dir, 'deps', package_name)
        ensure_dir(dep_dir)
        print('link :' + package_name)
        LocalCache.link(package_path, current_dir, package_name, 'include')
        LocalCache.link(package_path, current_dir, package_name, 'src')
        LocalCache.link(package_path, current_dir, package_name, 'ebin')

    @staticmethod
    def fill_package_from_temp(package: Package, path: str):
        package.fill_from_path(path)

    @staticmethod
    def link(package_path, current_dir, name, dir_to_link):
        include_src = join(package_path, dir_to_link)
        include_dst = join(current_dir, 'deps', name, dir_to_link)
        link_if_needed(include_src, include_dst)

    def __get_package_path(self, package: Package):
        namespace = package.url.split('/')[-2]
        return join(package.get_name(), namespace, package.vsn, self.erlang_version)
