import os
import subprocess
from abc import ABCMeta
from os.path import join
from subprocess import PIPE

from enot.packages.config.config import ConfigFile
from enot.tool.tool import AbstractTool
from enot.utils.file_utils import check_cmd, ensure_executable
from enot.utils.logger import critical, error, info, debug


def run_cmd(cmd: str or list, project: str, path: str,
            env_vars: dict or None = None, shell=False, output=PIPE) -> bool:
    debug(cmd)
    if env_vars is None:
        env_vars = dict(os.environ)
    ensure_runnable(cmd, path)
    p = subprocess.Popen(cmd, stdout=output, stderr=output, cwd=path, env=env_vars, shell=shell)
    if p.wait() != 0:
        critical(project + ' failed.')
        if output is not None:
            error(p.stderr.read().decode('utf8'))
            error(p.stdout.read().decode('utf8'))
        return False
    else:
        return True


def ensure_runnable(cmd: str, path: str):
    if isinstance(cmd, list):
        ensure_runnable(cmd[0], path)
    else:
        if cmd.startswith("./"):
            full_cmd = join(path, cmd.replace("./", ""))
            ensure_executable(full_cmd)


class AbstractCompiler(metaclass=ABCMeta):
    def __init__(self, package, executable='erlc'):
        self._package = package
        self._executable = executable
        self._tool = None

    @property
    def package(self):
        return self._package

    @property
    def project_name(self) -> str:
        return self.package.name

    @property
    def executable(self) -> str:
        return self._executable

    @property
    def root_path(self) -> str:  # Project path.
        return self.package.path

    @property
    def src_path(self) -> str:
        return join(self.package.path, 'src')

    @property
    def include_path(self) -> str:
        return join(self.package.path, 'include')

    @property
    def output_path(self) -> str:
        return join(self.package.path, 'ebin')

    @property
    def test_path(self) -> str:
        return join(self.package.path, 'test')

    @property
    def tool(self) -> AbstractTool or None:
        return self._tool

    @property
    def build_vars(self) -> list:
        return self.package.config.build_vars

    def compile(self, override_config: ConfigFile or None = None) -> bool:
        info(self.executable + ' build ' + self.project_name)
        return run_cmd(self.executable, self.project_name, self.root_path, output=None)

    def unit(self) -> bool:
        raise RuntimeError("Don't know how to run unit tests with " + self.executable)

    def common(self, log_dir: str) -> bool:
        raise RuntimeError("Don't know how to run common tests with " + self.executable)

    # find tool and link to project
    def ensure_tool(self, cache):
        if self.tool is None:  # no need to check tool for this compiler
            return
        maybe_tool = check_cmd(self.root_path, self.tool.name)
        if maybe_tool is True:  # found locally
            self._executable = self.tool.local_executable
            return
        elif maybe_tool:  # installed in system
            return
        elif maybe_tool is False:  # not found
            maybe_tool = self.__find_in_local(cache, self.tool)
        if maybe_tool is None:  # not found in local cache
            maybe_tool = self.__build_tool(cache, self.tool)
        self._executable = maybe_tool  # was linked to local dir

    def __find_in_local(self, cache, tool: AbstractTool):
        if cache.tool_exists(tool.name):  # tool is in local cache
            cache.link_tool(self.package, tool.name)
            return tool.local_executable
        return None

    def __build_tool(self, cache, tool: AbstractTool):
        path = cache.temp_dir
        tool_path = tool.ensure(path)
        cache.add_tool(tool.name, tool_path)
        cache.link_tool(self.package, tool.name)
        return tool.local_executable
