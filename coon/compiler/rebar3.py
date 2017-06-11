from os.path import join

from coon.compiler.rebar import RebarCompiler
from coon.packages.package import Package
from coon.utils.file_utils import copy_to


class Rebar3Compiler(RebarCompiler):
    def __init__(self, package: Package, executable='rebar3'):
        super().__init__(package, executable)

    def compile(self):
        if super().compile():
            copy_to(join(self.root_path, '_build', 'default', 'lib', self.project_name, 'ebin'),
                    join(self.root_path, 'ebin'))
            return True
        return False

    def unit(self) -> bool:
        return super().unit()

    def common(self, log_dir: str) -> bool:
        return super().common(log_dir)
