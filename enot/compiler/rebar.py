from enot.tool.rebar import RebarTool

from enot.packages.config.config import ConfigFile

from enot.utils.logger import info

from enot.compiler.abstract import AbstractCompiler, run_cmd


class RebarCompiler(AbstractCompiler):
    def __init__(self, package, executable='rebar'):
        super().__init__(package, executable)
        self._tool = RebarTool()

    def compile(self, override_config: ConfigFile or None = None):
        # self as ensure_tool in builder
        info('Rebar build ' + self.project_name)
        return run_cmd([self.executable, 'compile'], self.project_name, self.root_path)

    def unit(self) -> bool:
        return run_cmd([self.executable, 'eunit'], self.project_name, self.root_path)

    def common(self, log_dir: str) -> bool:  # TODO log_dir
        return run_cmd([self.executable, 'ct'], self.project_name, self.root_path)
