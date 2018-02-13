from os.path import join

from enot.compiler.rebar import RebarCompiler
from enot.packages.config.config import ConfigFile
from enot.tool.rebar3 import Rebar3Tool
from enot.utils.file_utils import copy_to


class Rebar3Compiler(RebarCompiler):
    def __init__(self, package, executable='rebar3'):
        super().__init__(package, executable)
        self._tool = Rebar3Tool()

    def compile(self, override_config: ConfigFile or None = None):
        if super().compile(override_config=override_config):
            copy_to(join(self.root_path, '_build', 'default', 'lib', self.project_name, 'ebin'),
                    join(self.root_path, 'ebin'))
            return True
        return False

    def unit(self) -> bool:
        return super().unit()

    def common(self, log_dir: str) -> bool:
        return super().common(log_dir)
