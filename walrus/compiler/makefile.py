import subprocess
from os.path import join
from subprocess import PIPE

from walrus.compiler import AbstractCompiler
from walrus.packages.config import ConfigFile


class MakefileCompiler(AbstractCompiler):
    def __init__(self, config: ConfigFile):
        super().__init__()
        self._src_path = join(config.path, 'src')
        self._include_path = join(config.path, 'include')
        self._output_path = join(config.path, 'ebin')
        self._root_path = config.path
        self._compose_app_file = config.compose_app_file
        self._project_name = config.name
        self._build_vars = config.build_vars

    def compile(self):
        p = subprocess.Popen('make', stdout=PIPE, stderr=PIPE, cwd=self._root_path)
        if p.wait() != 0:
            print(self._project_name + ' compilation failed: ')
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            return True
