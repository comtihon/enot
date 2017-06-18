import subprocess
from abc import ABCMeta
from os.path import join
from subprocess import PIPE

import os

from coon.packages.package import Package
from coon.utils.logger import critical, error, info, debug


def run_cmd(cmd: str or list, project: str, path: str,
            env_vars: dict or None = None, shell=False, output=False) -> bool:
    debug(cmd)
    if env_vars is None:
        env_vars = dict(os.environ)
    p = subprocess.Popen(cmd, stdout=PIPE, stderr=PIPE, cwd=path, env=env_vars, shell=shell)
    if p.wait() != 0:
        critical(project + ' failed.')
        error(p.stderr.read().decode('utf8'))
        error(p.stdout.read().decode('utf8'))
        return False
    else:
        if output:
            info(p.stdout.read().decode('utf8'))
        return True


class AbstractCompiler(metaclass=ABCMeta):
    def __init__(self, package: Package, executable='erlc'):
        self._package = package
        self._executable = executable

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
    def build_vars(self) -> list:
        return self.package.config.build_vars

    def compile(self) -> bool:
        info(self.executable + ' build ' + self.project_name)
        return run_cmd(self.executable, self.project_name, self.root_path)

    def unit(self) -> bool:
        raise RuntimeError("Don't know how to run unit tests with " + self.executable)

    def common(self, log_dir: str) -> bool:
        raise RuntimeError("Don't know how to run common tests with " + self.executable)
