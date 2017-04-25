import subprocess
from subprocess import PIPE

from walrus.compiler import AbstractCompiler


class BootstrapCompiler(AbstractCompiler):
    def compile(self):
        p = subprocess.Popen('./bootstrap', stdout=PIPE, stderr=PIPE, cwd=self.root_path)
        if p.wait() != 0:
            print(self.project_name + ' compilation failed: ')
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            return True
