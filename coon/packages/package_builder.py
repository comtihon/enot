import json
import os
from os import listdir
from os.path import join

from coon.compiler.compiler_factory import get_compiler
from coon.compiler.compiler_type import Compiler
from coon.compiler.relx import RelxCompiler
from coon.global_properties import GlobalProperties
from coon.packages.package import Package
from coon.tool.erlang_mk import ErlangMKTool
from coon.tool.rebar import RebarTool
from coon.tool.rebar3 import Rebar3Tool
from coon.tool.relxtool import RelxTool
from coon.tool.tool import AbstractTool
from coon.utils.file_utils import get_cmd, remove_dir
from coon.utils.logger import debug, info, warning


class Builder:
    def __init__(self, path: str, package: Package):
        super().__init__()
        self._system_config = GlobalProperties()
        self._path = path
        self._packages = {}
        self._project = package
        self._rescan_deps = False
        if self.system_config.compiler == Compiler.REBAR3:  # TODO refactor me somehow
            self.__ensure_rebar3()
        elif self.system_config.compiler == Compiler.REBAR:
            self.__ensure_rebar()
        elif self.system_config.compiler == Compiler.ERLANG_MK:
            self.__ensure_erlangmk()

    @classmethod
    def init_from_path(cls, path) -> 'Builder':
        package = Package.from_path(path)
        return cls(path, package)

    @classmethod
    def init_from_package(cls, path_to_package) -> 'Builder':
        package = Package.from_package(path_to_package)
        return cls(path_to_package, package)

    @property
    def path(self) -> str:  # path in system of building project
        return self._path

    @property
    def rescan_deps(self) -> bool:  # rescan deps if it is not prohibited
        return self.project.config.rescan_deps and self._rescan_deps

    @rescan_deps.setter
    def rescan_deps(self, rescan):  # mark rescan deps if not already set
        if not self.rescan_deps:
            self._rescan_deps = rescan

    @property
    def system_config(self) -> GlobalProperties:  # system configuration
        return self._system_config

    @property
    def packages(self) -> dict:  # all project's packages. key - package name, value - package
        return self._packages

    @property
    def project(self) -> Package:  # root package being built
        return self._project

    # Compose a package file
    def package(self):
        self.project.generate_package()

    # Run unit and common tests
    def unit_test(self) -> bool:
        compiler = get_compiler(self.system_config, self.project)
        return compiler.unit()

    def common_test(self, log_dir):
        compiler = get_compiler(self.system_config, self.project)
        return compiler.common(log_dir)

    # Parse package config, download missing deps to /tmp
    def populate(self, include_test_deps=False):
        deps = self.project.deps
        if include_test_deps:
            deps += self.project.test_deps
        self.__populate_deps(deps)
        locs = self.system_config.cache.local_cache.locks
        if locs:
            self.dump_locs(locs)

    # dump package's locs if there are some.
    # will dump only first level of locs. #TODO should I dump all?
    def dump_locs(self, locs: dict):
        filtered = {}
        for lock in locs.keys():
            [_, name] = lock.split('/')
            if name in self.project.config.deps.keys():
                filtered[lock] = locs[lock]
        if filtered != {}:
            with open(join(self.project.path, 'coon_locks.json'), 'w') as file:
                json.dump(filtered, file, sort_keys=True, indent=4)

    # if name is None - remove all locs. if set - remove only locs for name
    def drop_locs(self, name: str or None):  # drop locs for one or all packages
        cache = self.system_config.cache.local_cache
        if name is None:
            cache.locks = {}
        else:
            del cache.locks[name]
        with open(join(self.project.path, 'coon_locks.json'), 'w') as file:
            json.dump(cache.locks, file, sort_keys=True, indent=4)

    def add_package(self, remote: str, rewrite: bool, recurse: bool) -> bool:
        return self.system_config.cache.add_package(self.project, remote, rewrite, recurse)

    def build(self):
        build_res = self.__build_tree(self.project, is_subpackage=False)
        if self.rescan_deps:
            self.__rescan_deps()
        return build_res

    def deps(self):
        self.__build_deps(self.project, is_subpackage=False)
        if self.rescan_deps:
            self.__rescan_deps()

    def release(self):
        relx_path = self.__ensure_relx()
        return RelxCompiler(self.project, relx_path).compile()

    # Build all deps, add to cache and link to project
    def __build_deps(self, package: Package, is_subpackage=True):
        info('build deps for ' + package.name)
        is_first_line = package is not self.project
        if (is_subpackage and
                not self.project.config.link_all and
                self.system_config.cache.exists_local(package)):
            return True  # skip building dep's dep if not link all and dep was already built
        for dep in package.deps:
            if not self.system_config.cache.exists_local(dep):
                # if package not in cache - build and add to cache
                if not self.__build_tree(dep):
                    raise RuntimeError('Can\'t built dep ' + dep.name)
            if self.project.config.link_all and is_first_line:  # link dep's of deps if allowed
                upd = self.system_config.cache.link_package(dep, self.project.path)
                self.rescan_deps = upd
            upd = self.system_config.cache.link_package(dep, package.path)
            self.rescan_deps = upd
            self.__build_deps(dep)  # link all dep's deps (build them and add to cache if necessary)

    # Build package and it's deps, then add built package to local cache
    def __build_tree(self, package: Package, is_subpackage=True):
        self.__build_deps(package, is_subpackage)  # TODO add an ability to compile deps in parallel
        compiler = get_compiler(self.system_config, package)
        res = compiler.compile()
        if is_subpackage and res:
            self.system_config.cache.add_package_local(package)
        return res

    # TODO lock deps after fetched.
    def __populate_deps(self, level):  # TODO add an ability to fetch deps in parallel
        next_level = []
        for dep in level:
            if dep.name not in self.packages:
                debug('new dep: ' + dep.name)
                self.system_config.cache.populate(dep)  # populated dep becomes package
                self.packages[dep.name] = dep
                next_level += dep.deps
            else:
                next_level += self.__compare_and_select(dep)
        if next_level:
            self.__populate_deps(next_level)

    def __compare_and_select(self, dep: Package) -> list:
        pkg_vsn = self.packages[dep.name].git_vsn
        if dep.git_vsn != pkg_vsn:  # It is not the same dep
            try:  # try to compare versions
                [major1, minor1, bug1] = dep.git_vsn.split('.')
                [major2, minor2, bug2] = pkg_vsn.split('.')
                if major1 != major2:
                    raise RuntimeError(
                        'Deps ' + dep.name + ' has incompatible versions: ' + pkg_vsn + ' vs ' + dep.git_vsn)
                if minor1 > minor2 or bug1 > bug2:  # dep is newer than selected - prefer it
                    info('Prefer newer version for ' + dep.name + ', ' + pkg_vsn + ' -> ' + dep.git_vsn)
                    self.packages[dep.name] = dep
                    # TODO try use same repo to speed up new vsn fetch
                    self.system_config.cache.populate(dep)
                    dep.update_from_duplicate(self.packages[dep.name])
                    return dep.deps
            except ValueError:  # not m.m.b version (may be tag vs branch). Just replace.
                warning('Skip ' + dep.name + ' (' + dep.git_vsn + '). Use ' + pkg_vsn)
        dep.update_from_duplicate(self.packages[dep.name])
        return []

    # list deps directory and compare to packages, which should always be actual due to
    # populate at the beginning of the build. If dep is in deps dir, but not in self.packages
    # this dep is dead and should be unlinked.
    def __rescan_deps(self):
        deps_dir = join(self.project.path, 'deps')
        deps = listdir(deps_dir)
        for dep in deps:
            if dep not in self.packages:
                dep_path = join(deps_dir, dep)
                info('Drop dead dep: ' + dep_path)
                if os.path.islink(dep_path):
                    os.remove(dep_path)
                else:
                    remove_dir(dep_path)

    def __ensure_relx(self):
        return self.__ensure_tool(RelxTool())

    def __ensure_rebar3(self):
        return self.__ensure_tool(Rebar3Tool())

    def __ensure_rebar(self):
        return self.__ensure_tool(RebarTool())

    def __ensure_erlangmk(self):
        return self.__ensure_tool(ErlangMKTool())

    # Find binary. It can installed in the system, located in project directory, be in local cache.
    # If there is no - try to make it.
    def __ensure_tool(self, tool: AbstractTool):
        tool_path = get_cmd(self.path, tool.name)
        if tool_path is None:
            tool_path = self.__find_in_local(tool.name)
        if tool_path is None:
            tool_path = self.__build_tool(tool)
        if tool_path is None:
            raise RuntimeError("Can't obtain " + tool.name)
        return tool_path

    def __find_in_local(self, tool_name):
        if self.system_config.cache.local_cache.tool_exists(tool_name):  # tool is in local cache
            self.system_config.cache.local_cache.link_tool(self.project, tool_name)
            return './' + tool_name
        return None

    def __build_tool(self, tool: AbstractTool):
        path = self.system_config.cache.local_cache.temp_dir
        tool_path = tool.ensure(path)
        self.system_config.cache.local_cache.add_tool(tool.name, tool_path)
        self.system_config.cache.local_cache.link_tool(self.project, tool.name)
        return './' + tool.name
