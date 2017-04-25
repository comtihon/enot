import shutil
from os.path import join

import os
from git import Repo
from pkg_resources import Requirement, resource_filename

import coon
from coon.pac_cache import Cache
from coon.packages.package import Package
from coon.utils.file_utils import if_dir_exists, ensure_dir, link_if_needed, copy_file
from coon.utils.file_utils import remove_dir


class LocalCache(Cache):
    def __init__(self, temp_dir, conf):
        cache_url = conf['url']
        path = cache_url[7:]
        super().__init__(conf['name'], temp_dir, path)
        if not os.path.exists(path):
            os.makedirs(path)

    def exists(self, package: Package) -> bool:
        return if_dir_exists(self.path, self.__get_package_path(package))

    # clone git repo to tmp, make package to scan and update it's config
    def fetch_package(self, package: Package) -> bool:
        temp_path = join(self.temp_dir, package.name)
        print('fetch ' + temp_path)
        remove_dir(temp_path)
        repo = Repo.clone_from(package.url, temp_path)
        assert not repo.bare
        git = repo.git
        git.checkout(package.vsn)
        repo.create_head(package.vsn)
        LocalCache.fill_package_from_temp(package, temp_path)
        return True

    # add built package to local cache
    # TODO rebar3 built packages output in _build/...
    def add_package(self, package: Package, rewrite=False):
        full_dir = join(self.path, self.__get_package_path(package))
        ensure_dir(full_dir)
        print('add ' + package.name)
        path = package.config.path
        LocalCache.__copy_data(rewrite, full_dir, path, 'ebin')
        LocalCache.__copy_include(rewrite, full_dir, path)
        if package.config.with_source:
            LocalCache.__copy_data(rewrite, full_dir, path, 'src')
        if package.config.with_source and package.config.has_nifs:
            LocalCache.__copy_data(rewrite, full_dir, path, 'c_src')
        # TODO config from cache always have has_nifs false.
        if package.config.has_nifs:
            LocalCache.__copy_data(rewrite, full_dir, path, 'priv')
        # TODO include package file?
        resource = resource_filename(Requirement.parse(coon.APPNAME), 'coon/resources/EmptyMakefile')
        print('copy ' + resource + ' to ' + join(full_dir, 'Makefile'))
        copy_file(resource, join(full_dir, 'Makefile'))

    # link package from local cache to project
    def link_package(self, package: Package, package_path: str):
        if not package_path:
            package_path = os.getcwd()
        cache_path = join(self.path, self.__get_package_path(package))
        dep_dir = join(package_path, 'deps', package.name)
        ensure_dir(dep_dir)
        print('link ' + package.name)
        # TODO link everything found? (except package file)
        LocalCache.link(cache_path, package_path, package.name, 'Makefile')
        LocalCache.link(cache_path, package_path, package.name, 'include')
        LocalCache.link(cache_path, package_path, package.name, 'src')
        LocalCache.link(cache_path, package_path, package.name, 'ebin')
        if package.config.has_nifs:
            LocalCache.link(cache_path, package_path, package.name, 'priv')

    # link custom dir/file from local cache to path
    def link_custom(self, package: Package, path: str, custom: str):
        cache_path = join(self.path, self.__get_package_path(package))
        LocalCache.link(cache_path, path, package.name, custom)

    def __get_package_path(self, package: Package):
        namespace = package.url.split('/')[-2]
        return join(namespace, package.name, package.vsn, self.erlang_version)

    @staticmethod
    def fill_package_from_temp(package: Package, path: str):
        package.fill_from_path(path)

    # TODO relinking deps on vsn switching
    @staticmethod
    def link(cache_path, package_path, name, dir_to_link):
        include_src = join(cache_path, dir_to_link)
        include_dst = join(package_path, 'deps', name, dir_to_link)
        link_if_needed(include_src, include_dst)

    @staticmethod
    def __copy_include(rewrite, full_dir, path):
        cache_include = join(full_dir, 'include')
        if rewrite or not os.path.exists(cache_include):
            package_include = join(path, 'include')
            if os.path.exists(package_include):
                print('copy ' + package_include + ' to' + cache_include)
                shutil.copytree(package_include, cache_include)

    @staticmethod
    def __copy_data(rewrite, full_dir, path, source_dir):
        cache_src = join(full_dir, source_dir)
        if rewrite or not os.path.exists(cache_src):
            package_src = join(path, source_dir)
            print('copy ' + package_src + ' to' + cache_src)
            shutil.copytree(package_src, cache_src)
