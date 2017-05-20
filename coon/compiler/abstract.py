import subprocess
from abc import ABCMeta
from os.path import join
from subprocess import PIPE

from coon.packages.package import Package


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
    def build_vars(self) -> list:
        return self.package.config.build_vars

    def compile(self) -> bool:
        p = subprocess.Popen(self.executable, stdout=PIPE, stderr=PIPE, cwd=self.root_path)
        if p.wait() != 0:
            print(self.project_name + ' compilation failed: ')
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            return True
