import subprocess
from subprocess import PIPE

from coon.compiler import AbstractCompiler


class ErlangMKCompiler(AbstractCompiler):
    def compile(self):
        # TODO ensure Makefile and ensure it includes erlang.mk
        p = subprocess.Popen('make', stdout=PIPE, stderr=PIPE, cwd=self.root_path)
        if p.wait() != 0:
            print(self.project_name + ' compilation failed: ')
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            return True
