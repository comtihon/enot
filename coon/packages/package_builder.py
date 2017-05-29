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
from coon.utils.file_utils import get_cmd


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

    # Parse package config, download missing deps to /tmp
    def populate(self):
        self.__populate_deps(self.project.deps)

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
        print('build deps for ' + package.name)
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
    def __populate_deps(self, level):  # TODO add an ability to fetc deps in parallel
        next_level = []
        for dep in level:
            if dep.name not in self.packages:
                print('new dep: ' + dep.name)
                self.system_config.cache.populate(dep)  # populated dep becomes package
                self.packages[dep.name] = dep
                next_level += dep.deps
            else:
                pkg_vsn = self.packages[dep.name].vsn
                if dep.vsn != pkg_vsn:  # Warn only if it is not the same dep
                    print('Skip ' + dep.name + ' (' + dep.vsn + '). Use ' + pkg_vsn)
        if next_level:
            self.__populate_deps(next_level)

    def __rescan_deps(self):
        deps = listdir(join(self.project.path, 'deps'))
        # TODO for all listed deps not in self.packages - remove.

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
