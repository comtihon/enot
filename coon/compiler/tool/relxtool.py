import subprocess
from os.path import join
from subprocess import PIPE

from coon.compiler import Compiler
from coon.compiler.tool import AbstractTool


class RelxTool(AbstractTool):
    def __init__(self, rebar3path: str) -> None:
        super().__init__()
        self._rebar3path = rebar3path

    @property
    def rebar3path(self) -> str:
        return self._rebar3path

    @property
    def url(self) -> str:
        return 'https://github.com/erlware/relx.git'

    @property
    def version(self) -> str:
        return 'v3.22.4'

    @property
    def name(self) -> str:
        return 'relx'

    @property
    def compiler(self):
        return Compiler.REBAR3

    def build(self, src_path: str) -> str or None:
        executable = self.rebar3path
        u = subprocess.Popen([executable, 'update'], stdout=PIPE, stderr=PIPE, cwd=src_path)
        if u.wait() != 0:
            print(u.stderr.read().decode('utf8'))
            print(u.stdout.read().decode('utf8'))
            return None
        else:
            s = subprocess.Popen([executable, 'as escript escriptize'], stdout=PIPE, stderr=PIPE, cwd=src_path)
            if s.wait() != 0:
                print(s.stderr.read().decode('utf8'))
                print(s.stdout.read().decode('utf8'))
                return None
            return join(src_path, '_build/default/bin/relx')
