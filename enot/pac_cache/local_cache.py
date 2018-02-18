import json
import os
import shutil
import stat
from os import listdir
from os.path import join

from git import Repo
from pkg_resources import Requirement, resource_filename

import enot
from enot.pac_cache.cache import Cache, CacheType
from enot.packages.package import Package
from enot.utils.file_utils import if_dir_exists, ensure_dir, link_if_needed, copy_file
from enot.utils.file_utils import remove_dir
from enot.utils.logger import debug, info


class LocalCache(Cache):
    def __init__(self, temp_dir: str, default_erlang: str, conf):
        cache_url = conf['url']
        path = cache_url[7:]
        super().__init__(conf['name'], temp_dir, path, default_erlang, CacheType.LOCAL)
        if not os.path.exists(path):
            os.makedirs(path)
        ensure_dir(temp_dir)
        ensure_dir(self.tool_dir)
        self._locks = {}
        self.__fill_locks()

    @property
    def tool_dir(self):
        return join(self.path, 'tool')

    @property
    def locks(self) -> dict:
        return self._locks

    @locks.setter
    def locks(self, locks: dict):
        self._locks = locks

    def get_lock(self, name: str) -> str or None:
        return self.locks.get(name, None)

    def set_lock(self, dep: Package, hash_str: str):
        self._locks[dep.fullname] = dep.git_branch + '-' + hash_str

    def get_package_path(self, package: Package, no_null=False) -> str or None:
        if package.git_tag is not None:  # normal tagged dep
            return join(package.fullname, package.git_vsn, self.erlang_version)
        lock = self.get_lock(package.fullname)
        if lock is not None:  # locked branch dep
            return join(package.fullname, lock, self.erlang_version)
        if no_null and package.git_branch is not None:
            return join(package.fullname, package.git_vsn, self.erlang_version)
        return None  # unlocked branch dep, should be fetched

    def exists(self, package: Package) -> bool:
        return self.check_exists(self.get_package_path(package))

    def check_exists(self, path: str) -> bool:
        debug('check ' + self.path + ' ' + str(path))
        return if_dir_exists(self.path, path) is not None

    def tool_exists(self, toolname: str) -> bool:
        return os.path.exists(join(self.tool_dir, toolname))

    # clone git repo to tmp, make package to scan and update it's config
    def fetch_package(self, dep: Package):
        temp_path = join(self.temp_dir, dep.name)
        info('fetch ' + temp_path)
        remove_dir(temp_path)
        vsn, need_lock = self.__get_vsn(dep)
        hash_str = LocalCache.fetch(dep.url, vsn, temp_path)
        dep.update_from_cache(temp_path)
        if need_lock:
            self.set_lock(dep, hash_str)

    # add built package to local cache, update its path
    def add_package(self, package: Package, rewrite=False) -> bool:
        full_dir = join(self.path, self.get_package_path(package, True))
        ensure_dir(full_dir)
        info('add ' + package.fullname)
        path = package.path
        LocalCache.__copy_data(rewrite, full_dir, path, 'ebin')
        LocalCache.__copy_include(rewrite, full_dir, path)
        if package.config.with_source:
            LocalCache.__copy_data(rewrite, full_dir, path, 'src')
        if package.config.with_source and package.has_nifs:
            LocalCache.__copy_data(rewrite, full_dir, path, 'c_src')
        if os.path.exists(join(path, 'priv')):
            LocalCache.__copy_data(rewrite, full_dir, path, 'priv')
        enot_package = join(path, package.name + '.ep')
        if not os.path.isfile(enot_package):
            debug('generate missing package')
            package.generate_package()
        copy_file(join(path, 'enot_config.json'), join(full_dir, 'enot_config.json'))
        copy_file(enot_package, join(full_dir, package.name + '.ep'))
        resource = resource_filename(Requirement.parse(enot.APPNAME), 'enot/resources/EmptyMakefile')
        debug('copy ' + resource + ' to ' + join(full_dir, 'Makefile'))
        copy_file(resource, join(full_dir, 'Makefile'))
        package.path = full_dir  # update package's dir to point to cache
        return True

    def get_erl_versions(self, fullname: str, version: str) -> list:
        vsn_path = join(self.path, fullname, version)
        if not os.path.exists(vsn_path):
            return []
        return listdir(vsn_path)

    def get_versions(self, fullname: str) -> list:
        name_path = join(self.path, fullname)
        if not os.path.exists(name_path):
            return []
        return listdir(name_path)

    def add_tool(self, toolname: str, toolpath: str):
        info('add ' + toolname)
        tool_dst = join(self.tool_dir, toolname)
        copy_file(toolpath, tool_dst)
        st = os.stat(tool_dst)
        os.chmod(tool_dst, st.st_mode | stat.S_IEXEC)

    # link package from local cache to project
    # return true if link was changed (dep was updated)
    def link_package(self, package: Package, dest_path: str) -> bool:
        if not dest_path:
            dest_path = os.getcwd()
        cache_path = join(self.path, self.get_package_path(package))
        dep_dir = join(dest_path, 'deps', package.name)
        ensure_dir(dep_dir)
        debug('link ' + package.name)
        changed = []
        for file in listdir(cache_path):
            if file != package.name + '.ep':
                changed.append(LocalCache.link(cache_path, dest_path, package.name, file))
        return all(changed)  # if all links were changed - it is a new version

    def link_tool(self, package: Package, toolname: str):
        cache_path = join(self.tool_dir, toolname)
        link_if_needed(cache_path, join(package.path, toolname))

    # load package's locks.
    def __fill_locks(self):
        if os.path.isfile('enot_locks.json'):
            with open('enot_locks.json', 'r') as file:
                self._locks = json.load(file)

    # Return git version to use and should lock flag
    def __get_vsn(self, dep: Package):
        if dep.git_tag:  # no need to check lock over tag version
            return dep.git_tag, False
        lock = self.get_lock(dep.name)
        if lock:  # this package's version is locked, return locked commit's hash
            [branch, hash_str] = lock.split('-')
            if branch == dep.git_branch:  # same branch locked
                return hash_str, False
            return dep.git_branch, True  # locked branch was changed
        return dep.git_branch, True

    @staticmethod
    def link(cache_path: str, package_path: str, name: str, dir_to_link: str) -> bool:
        include_src = join(cache_path, dir_to_link)
        include_dst = join(package_path, 'deps', name, dir_to_link)
        return link_if_needed(include_src, include_dst)

    @staticmethod
    def fetch(url, rev, path):
        repo = Repo.clone_from(url, path)
        if repo.bare:
            raise RuntimeError('Empty repo ' + url)
        git = repo.git
        git.checkout(rev)
        repo.create_head(rev)
        return repo.head.object.hexsha

    @staticmethod
    def __copy_include(rewrite, full_dir, path):
        cache_include = join(full_dir, 'include')
        if rewrite or not os.path.exists(cache_include):
            package_include = join(path, 'include')
            if os.path.exists(package_include):
                debug('copy ' + package_include + ' to' + cache_include)
                shutil.copytree(package_include, cache_include)

    @staticmethod
    def __copy_data(rewrite, full_dir, path, source_dir):
        cache_src = join(full_dir, source_dir)
        if rewrite or not os.path.exists(cache_src):
            package_src = join(path, source_dir)
            debug('copy ' + package_src + ' to ' + cache_src)
            shutil.copytree(package_src, cache_src)
