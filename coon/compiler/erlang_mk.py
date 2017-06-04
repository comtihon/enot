import subprocess

import logging

import coon
from subprocess import PIPE
from coon.compiler.abstract import AbstractCompiler, run_cmd
from coon.packages.package import Package


class ErlangMKCompiler(AbstractCompiler):
    def __init__(self, package: Package, executable='make'):
        super().__init__(package, executable)
        # TODO ensure Makefile and ensure it includes erlang.mk

    def common(self, log_dir: str) -> bool:  # TODO log_dir
        return self.__run_test('ct')

    def unit(self) -> bool:
        return self.__run_test('eunit')

    def __run_test(self, test: str):
        return run_cmd([self.executable, test], self.project_name, self.root_path)
