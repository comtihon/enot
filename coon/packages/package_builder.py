from coon.compiler.relx import RelxCompiler
from coon.compiler.compiler_factory import get_compiler
from coon.compiler.compiler_type import Compiler
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
    def init_from_package(cls, path_to_package):
        package = Package.from_package(path_to_package)
        return cls(path_to_package, package)

    @property
    def path(self) -> str:  # path in system of building project
        return self._path

    @property
    def system_config(self) -> GlobalProperties:  # system configuration
        return self._system_config

    @property
    def packages(self) -> dict:  # all project's packages
        return self._packages

    @property
    def project(self) -> Package:  # root package being built
        return self._project

    # Compose a package file
    def package(self):
        self.system_config.cache.package(self.project)

    # Parse package config, download missing deps to /tmp
    def populate(self):
        self.__populate_deps(self.project.dep_packages.values())

    def add_package(self, remote, rewrite):
        self.system_config.cache.add_package(self.project, remote, rewrite)

    def build(self):
        return self.__build_tree(self.project, is_subpackage=False)

    def deps(self):
        self.__build_deps(self.project, is_subpackage=False)

    # TODO check if there is a need to compile first.
    def release(self):
        relx_path = self.__ensure_relx()
        return RelxCompiler(self.project, relx_path).compile()

    # TODO check all included in release apps for presence in deps
    # TODO take applications from app.src and add to relx.config
    def __check_app_deps(self, deps):
        # TODO ensure, if deps in relx.config are needed
        return

    # Build all deps, add to cache and link to project
    def __build_deps(self, package: Package, is_subpackage=True):
        # TODO check if package.config.path exists (if deps were populated before calling build_deps/build_tree)
        print('check ' + package.name)
        if is_subpackage and self.system_config.cache.exists(package):
            return True
        for dep in package.list_deps():
            if not self.system_config.cache.exists(dep):  # if package not in cache - build and add to cache
                if not self.__build_tree(dep):
                    raise RuntimeError('Can\'t built dep ' + dep.name)
            self.system_config.cache.link_package(dep, package.config.path)

    # Build package and it's deps
    def __build_tree(self, package: Package, is_subpackage=True):
        self.__build_deps(package, is_subpackage)  # TODO add an ability to compile deps in parallel
        compiler = get_compiler(self.system_config, package)
        res = compiler.compile()
        if is_subpackage and res:
            self.system_config.cache.add_package_local(package)
        return res

    # TODO lock deps after fetched.
    def __populate_deps(self, level):  # TODO add an ability to operate with deps in parallel
        next_level = []
        for dep in level:
            if dep.name not in self.packages:
                print('new dep: ' + dep.name)
                self.packages[dep.name] = dep.vsn
                if not self.system_config.cache.exists(dep):
                    self.system_config.cache.fetch_package(dep)
                next_level += dep.dep_packages.values()
            else:
                if dep.vsn != self.packages[dep.name]:  # Warn only if it is not the same dep
                    print('Skip ' + dep.name + ' (' + dep.vsn + '). Use ' + self.packages[dep.name])
        if next_level:
            self.__populate_deps(next_level)

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
