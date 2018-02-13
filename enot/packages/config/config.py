import json
from abc import ABCMeta, abstractmethod
from os.path import join
from urllib import request

from enot.compiler.compiler_type import Compiler
from enot.packages.dep import Dep
from enot.utils.file_utils import write_file

"""
Project config file. Can be rebar.config (Rebar1-3), config in Makefile (erlang.mk) or enot_config.json (enot)
"""


def write_package_config(path, package_config):
    write_file(join(path, 'enot_config.json'), package_config)


def request_hex_info(name: str) -> dict:
    package = request.urlopen('https://hex.pm/api/packages/' + name)
    return json.loads(package.read().decode())


def get_dep_info_from_hex(name: str, tag: str) -> Dep:
    parsed = request_hex_info(name)
    meta = parsed['meta']
    links = meta['links']
    links_lower = {k.lower(): v for k, v in links.items()}  # sometimes Hex have GitHub and sometimes Github it response
    url = links_lower['github']
    return Dep(url, None, tag=tag)


class ConfigFile(metaclass=ABCMeta):
    def __init__(self):
        self._prebuild = []
        self._build_vars = []
        self._c_build_vars = []
        self._deps = {}
        self._test_deps = {}
        self._with_source = True
        self._name = ''
        self._conf_vsn = None
        self._git_tag = None
        self._git_branch = None
        self._url = None
        self._link_all = True
        self._rescan_deps = True
        self._fullname = None
        self._auto_build_order = True
        self._override_conf = False
        self._disable_prebuild = False
        self._erlang_versions = []
        self._compare_versions = True
        self._install = []
        self._uninstall = []

    @property
    def name(self) -> str:  # project's name
        return self._name

    @name.setter
    def name(self, name):
        self._name = name

    @property
    def with_source(self) -> bool:  # include source, when adding to local cache
        return self._with_source

    @property
    def conf_vsn(self) -> str or None:  # version from config
        return self._conf_vsn

    @property
    def git_tag(self) -> str or None:  # git tag
        return self._git_tag

    @git_tag.setter
    def git_tag(self, tag):
        self._git_tag = tag

    @property
    def fullname(self) -> str or None:  # namespace/name
        return self._fullname

    @fullname.setter
    def fullname(self, fullname):
        self._fullname = fullname

    @property
    def compare_versions(self) -> bool:
        return self._compare_versions

    @property
    def auto_build_order(self) -> bool:  # should analyse sources during compilation
        return self._auto_build_order

    @property
    def override_conf(self) -> bool:  # should override deps configuration
        return self._override_conf

    @property
    def disable_prebuild(self) -> bool:  # should skip prebuild
        return self._disable_prebuild

    @property
    def git_branch(self) -> str:
        return self._git_branch

    @git_branch.setter
    def git_branch(self, branch: str):
        self._git_branch = branch

    @property
    def deps(self) -> dict:  # deps from config file. Dict of Dep, where keys are their names
        return self._deps

    @property
    def test_deps(self) -> dict:
        return self._test_deps

    @property
    def erlang_versions(self) -> list:
        return self._erlang_versions

    @property
    def prebuild(self) -> list:  # actions to be run before the build
        return self._prebuild

    @property
    def build_vars(self) -> list:  # erlang build vars, passed to the compiler (string or tuple of size 2)
        return self._build_vars

    @property
    def c_build_vars(self) -> list:  # c build vars. list of kv tuples
        return self._c_build_vars

    @property
    def link_all(self) -> bool:  # link dep's deps to primary project
        return self._link_all

    @property
    def rescan_deps(self) -> bool:  # rescan dep tree on dep update detected
        return self._rescan_deps

    @property
    def url(self) -> str:
        return self._url

    @url.setter
    def url(self, url):
        self._url = url

    @property
    def install(self) -> list:
        return self._install

    @property
    def uninstall(self) -> list:
        return self._uninstall

    @property
    def is_release(self) -> bool:
        return False

    @abstractmethod
    def get_compiler(self) -> Compiler:
        pass

    def need_enotify(self):
        return True

    # fullname can be set from enot_config.json fullname or by parsing local git url
    # while setting up Package
    def fullname_from_git(self, url: str):
        self._fullname = join(url.split('/')[-2], url.split('/')[-1])

    def export(self) -> dict:
        export = {'with_source': self.with_source,
                  'auto_build_order': self.auto_build_order,
                  'override': self.override_conf,
                  'disable_prebuild': self.disable_prebuild}
        if self.build_vars:
            export['build_vars'] = self.build_vars
        if self.c_build_vars:
            export['c_build_vars'] = self.c_build_vars
        if self.prebuild:
            prebuild = [pb.export() for pb in self.prebuild]
            export['prebuild'] = prebuild
        if self.install:
            install = [action.export() for action in self.install]
            export['install'] = install
        if self.uninstall:
            uninstall = [action.export() for action in self.uninstall]
            export['uninstall'] = uninstall
        if self.fullname:
            export['fullname'] = self.fullname
        if self.git_tag is not None:
            export['tag'] = self.git_tag
        if self.git_branch is not None:
            export['branch'] = self.git_branch
        if self.url is not None:
            export['url'] = self.url
        if self.erlang_versions is not None:
            export['erlang'] = self.erlang_versions
        return export
