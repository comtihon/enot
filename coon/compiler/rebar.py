from coon.utils.logger import info

from coon.compiler.abstract import AbstractCompiler, run_cmd
from coon.packages.package import Package


class RebarCompiler(AbstractCompiler):
    def __init__(self, package: Package, executable='rebar'):
        super().__init__(package, executable)

    def compile(self):
        # TODO check rebar existence in system or in ./
        # self as ensure_tool in builder
        info('Rebar build ' + self.project_name)
        return run_cmd([self.executable, 'compile'], self.project_name, self.root_path)

    def unit(self) -> bool:
        return run_cmd([self.executable, 'eunit'], self.project_name, self.root_path)

    def common(self, log_dir: str) -> bool:  # TODO log_dir
        return run_cmd([self.executable, 'ct'], self.project_name, self.root_path)
