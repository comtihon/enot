import os
from os import listdir

from enot.compiler.abstract import AbstractCompiler, run_cmd
from enot.packages.config.config import ConfigFile
from enot.tool.erlang_mk import ErlangMKTool
from enot.utils.logger import warning


class ErlangMKCompiler(AbstractCompiler):
    def __init__(self, package, executable='make'):
        super().__init__(package, executable)
        self._tool = ErlangMKTool()
        self._retry = 5
        # TODO ensure Makefile includes erlang.mk

    # number of retries for success compilation
    # this is because ErlangMK sometimes looses files, but report success compilation
    def retry(self) -> int:
        return self._retry

    def compile(self, override_config: ConfigFile or None = None) -> bool:
        if self.retry == 0:
            return False
        if super().compile(override_config):
            if not self.check_output():  # compilation was success but some files were not compiled
                warning('Retry compilation for ' + self.project_name)
                self._retry -= 1
                run_cmd(['make', 'clean'], self.project_name, self.root_path, output=None)
                return self.compile(override_config=override_config)
            return True
        return False

    def common(self, log_dir: str) -> bool:  # TODO log_dir
        return self.__run_test('ct')

    def unit(self) -> bool:
        return self.__run_test('eunit')

    def check_output(self):
        source_files = self.__get_source_files()
        output = self.__get_compiled_files()
        for source in source_files:
            if source not in output:
                warning('ErlangMK: ' + source + ' file was not compiled')
                return False
        return True

    def __get_source_files(self):
        source_files = []
        for root, directories, filenames in os.walk(self.src_path):
            for filename in filenames:
                if filename.endswith('.erl'):
                    source_files.append(filename[:-4])
        return source_files

    def __get_compiled_files(self):
        output = listdir(self.output_path)
        return [out[:-5] for out in output if out.endswith('.beam')]

    def __run_test(self, test: str):
        return run_cmd([self.executable, test], self.project_name, self.root_path)
