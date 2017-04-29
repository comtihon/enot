import subprocess
from os.path import join
from subprocess import PIPE

from coon.compiler import Compiler
from coon.compiler.tool import AbstractTool


class RebarTool(AbstractTool):
    @property
    def url(self) -> str:
        return 'https://github.com/rebar/rebar.git'

    @property
    def version(self) -> str:
        return 'RELEASE-1'

    @property
    def name(self) -> str:
        return 'rebar'

    @property
    def compiler(self):
        return Compiler.BOOTSTRAP

    def build(self, src_path: str) -> str or None:
        executable = './' + self.compiler.value
        u = subprocess.Popen(executable, stdout=PIPE, stderr=PIPE, cwd=src_path)
        if u.wait() != 0:
            print(u.stderr.read().decode('utf8'))
            print(u.stdout.read().decode('utf8'))
            return None
        else:
            return join(src_path, self.name)
