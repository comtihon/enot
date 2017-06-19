from coon.packages.config.config import ConfigFile

from coon.utils.logger import info

from coon.tool.erlang_mk import ErlangMKTool

from coon.compiler.abstract import AbstractCompiler, run_cmd
from coon.packages.package import Package


class ErlangMKCompiler(AbstractCompiler):
    def __init__(self, package: Package, executable='make'):
        super().__init__(package, executable)
        self._tool = ErlangMKTool()
        # TODO ensure Makefile includes erlang.mk

    def common(self, log_dir: str) -> bool:  # TODO log_dir
        return self.__run_test('ct')

    def unit(self) -> bool:
        return self.__run_test('eunit')

    def __run_test(self, test: str):
        return run_cmd([self.executable, test], self.project_name, self.root_path)
