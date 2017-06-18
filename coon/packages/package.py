import json
import os
import tarfile
from os.path import join

from git import Repo, InvalidGitRepositoryError

from coon.packages.application_config import AppConfig
from coon.packages.config import config_factory
from coon.packages.config.config import ConfigFile
from coon.packages.config.coon import CoonConfig
from coon.packages.config.dep_config import DepConfig
from coon.packages.dep import Dep
from coon.utils.file_utils import tar
from coon.utils.logger import info


class Package:
    def __init__(self, path: str, config: ConfigFile or None, app_config: AppConfig or None, has_nifs=False):
        self._config = config
        self._app_config = app_config
        self._path = path
        self._has_nifs = has_nifs
        self._deps = []
        self._test_deps = []
        self.__set_deps()
        self.__set_url_from_git()
        self.__set_git_vsn()

    @property
    def name(self) -> str:
        if self.app_config:
            return self.app_config.name
        return self.config.name

    @property
    def fullname(self) -> str:  # namespace/name
        return self.config.fullname

    @property
    def url(self) -> str or None:  # git url
        return self.config.url

    @property
    def git_branch(self) -> str:  # git branch
        return self.config.git_branch

    @property
    def git_tag(self) -> str or None:  # git tag
        return self.config.git_tag

    @property
    def git_vsn(self) -> str:  # prefer tag, but if None - return branch name
        if self.git_tag:
            return self.git_tag
        return self.git_branch

    @property
    def vsn(self) -> str:  # package version from app.src or from config (preferred)
        if self.config.conf_vsn:
            return self.config.conf_vsn
        return self.app_config.vsn

    @property
    def path(self) -> str:  # packages path
        return self._path

    @path.setter
    def path(self, path):
        self._path = path

    @property
    def app_config(self) -> AppConfig or None:  # .app.src or .app
        return self._app_config

    @property
    def config(self) -> ConfigFile:  # ConfigFile
        return self._config

    @property
    def deps(self) -> list:  # package's deps.
        return self._deps

    @property
    def test_deps(self) -> list:
        return self._test_deps

    @property
    def std_apps(self) -> list:  # standard erlang apps. Used in app.src templates
        return ['kernel', 'stdlib']

    @property
    def apps(self) -> list:  # appliations, which should be run before this app
        apps = [dep.name for dep in self.deps]
        if self.app_config and self.app_config.applications:
            apps += self.app_config.applications
        return list(set(apps))

    # package path contains c_src folder or priv/<project_name>.so (in case of local cache without c_src)
    @property
    def has_nifs(self) -> bool:
        return self._has_nifs

    @classmethod  # TODO url is not set here!
    # Package is created from path on local system. Usually when opening project
    def from_path(cls, path: str, url=None):
        config = config_factory.read_project(path, url=url)
        app_config = AppConfig.from_path(path)
        has_nifs = os.path.exists(join(path, 'c_src'))  # TODO search for .so files in priv?
        return cls(path, config, app_config, has_nifs)

    @classmethod
    # Package is created from coon package archive.
    # Usually when downloading cp file from remote cache or manually uploading a package
    def from_package(cls, path: str, url=None):  # TODO url is not set here!
        project_path, has_nifs, config, app_config = do_update_from_package(path, url)
        return cls(project_path, config, app_config, has_nifs)

    @classmethod
    # Package is created from dependency name, (url, vsn), got from config file deps
    # Usually when populating package's deps
    def from_dep(cls, name: str, dep: Dep):
        return cls(None, DepConfig(name, dep), None, False)

    # Update Package, created by from_dep classmethod.
    # Is called, when dep was fetched by local to some path and need to be fully filled
    def update_from_cache(self, path: str):
        name = self.name
        git_tag = self.git_tag
        git_branch = self.git_branch
        self._config = config_factory.read_project(path, url=self.url)
        self._config.git_tag = git_tag
        self._config.git_branch = git_branch
        if self.config.name == '':
            self.config.name = name
        if not self.fullname:
            self.config.fullname_from_git(self.url, self.name)
        self._app_config = AppConfig.from_path(path)
        self._has_nifs = os.path.exists(join(path, 'c_src'))
        self._path = path
        self.__set_deps()

    # Update Package, created by from_dep classmethod.
    # Is called, when dep coon package was fetched by remote cache and need to be fully filled
    def update_from_package(self, path: str):
        name = self.name
        git_tag = self.git_tag
        git_branch = self.git_branch
        project_path, has_nifs, config, app_config = do_update_from_package(path, self.url)
        self.path = project_path
        self._config = config
        self._app_config = app_config
        self._has_nifs = has_nifs
        self.config.git_tag = git_tag
        self.config.git_branch = git_branch
        if self.config.name == '':
            self.config.name = name
        if not self.fullname:
            self.config.fullname_from_git(self.url, self.name)

    # If package has dep and this dep has already be populated
    # becoming real package - update_from_duplicate should be called
    # on duplicate to get values from populated dep, such as path,
    # configs.
    def update_from_duplicate(self, package: 'Package'):
        self._config = package.config
        self._app_config = package.app_config
        self._path = package.path
        self._deps = package.deps
        self._has_nifs = package.has_nifs

    def export(self) -> dict:
        export = self.config.export()
        export['name'] = self.name
        export['deps'] = [dep.export() for dep in self.deps]
        if self.vsn is not None:
            export['app_vsn'] = self.vsn
        return export

    def generate_package(self):
        pack_dir = self.path
        exported = self.export()
        config = join(pack_dir, 'coonfig.json')
        with open(config, 'w') as outfile:
            json.dump(exported, outfile, sort_keys=True, indent=4)
        dirs_to_add = []
        add_if_exist(pack_dir, 'ebin', dirs_to_add)
        add_if_exist(pack_dir, 'priv', dirs_to_add)
        if self.config.with_source:
            add_if_exist(pack_dir, 'src', dirs_to_add)
            add_if_exist(pack_dir, 'include', dirs_to_add)
            add_if_exist(pack_dir, 'c_src', dirs_to_add)
        add_if_exist(pack_dir, 'coonfig.json', dirs_to_add)
        add_if_exist(pack_dir, 'coon_locks.json', dirs_to_add)
        package_dst = join(pack_dir, self.name + '.cp')
        info('create package ' + package_dst)
        tar(pack_dir, dirs_to_add, package_dst)

    def __set_deps(self):
        if self.config:  # TODO check config.drop_unknown (if not a template)
            for name, dep in self.config.deps.items():
                self._deps.append(Package.from_dep(name, dep))
            for name, dep in self.config.test_deps.items():
                self._test_deps.append(Package.from_dep(name, dep))

    def __set_url_from_git(self):
        if not self.url:
            try:
                repo = Repo(self.path)
                self.config.url = repo.remotes.origin.url  # TODO remove .git ending?
            except InvalidGitRepositoryError:
                return
        if not self.fullname:
            self.config.fullname_from_git(self.url, self.name)

    def __set_git_vsn(self):
        if not self.git_tag and self.path:
            try:
                repo = Repo(self.path)
                tag_name = None
                for tag in repo.tags:
                    if tag.commit == repo.head.commit:
                        tag_name = tag.path
                if tag_name:
                    paths = tag_name.split('/')
                    [tag] = paths[-1:]
                    self.config.git_tag = tag
                self.config.git_branch = repo.active_branch.name
            except (InvalidGitRepositoryError, TypeError):
                return


def add_if_exist(src_dir, dir_to_add, dirs):
    if os.path.exists(join(src_dir, dir_to_add)):
        dirs.append(dir_to_add)


def do_update_from_package(path: str, url: str or None) -> (str, bool, ConfigFile, AppConfig or None):
    paths = path.split('/')
    package_name = paths[len(paths) - 1]
    project_path = '/'.join(paths[:-1])
    with tarfile.open(path) as pack:
        config = CoonConfig.from_package(pack, url)
        names = pack.getnames()
        conf_app_src = package_name + '.app.src'
        conf_app = package_name + '.app'
        if conf_app_src in names:
            app_config = AppConfig.from_package(conf_app_src, pack)
        elif conf_app in names:
            app_config = AppConfig.from_package(conf_app, pack, compose=False)
        else:
            app_config = None
        has_nifs = 'c_src' in names
    return project_path, has_nifs, config, app_config
