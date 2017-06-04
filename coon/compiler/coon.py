import socket
from os import listdir
from os.path import isfile, join, isdir

import os
from jinja2 import Template

from coon.compiler.abstract import AbstractCompiler, run_cmd
from coon.compiler.c_compiler import CCompiler
from coon.utils.file_utils import ensure_dir, read_file
from coon.utils.logger import debug, info


def check_extension(file: str, extension: str) -> bool:
    return isfile(file) and file.split('.')[-1] == extension


# If parse-transform is found in file and this transform is in modules to be compiled
# - should compile it before them.
def parse_transform_first(first: dict, files: dict, file):
    for line in file:
        stripped = line.replace(' ', '')
        if stripped.startswith('-compile([{parse_transform,'):
            cropped = stripped[len('-compile([{parse_transform,'):]
            [transfrom_module, _] = cropped.split('}')
            if transfrom_module in files.keys():
                first[transfrom_module] = files[transfrom_module]
                break


class CoonCompiler(AbstractCompiler):
    @property
    def deps_path(self) -> str:
        return join(self.package.path, 'deps')

    def compile(self) -> bool:
        info('build ' + self.project_name)
        self.__run_prebuild()
        all_files = self.__get_all_files(self.src_path, 'erl')
        first_compiled = CoonCompiler.form_compilation_order(all_files)
        debug('ensure ' + self.output_path)
        ensure_dir(self.output_path)
        res = True
        if self.package.has_nifs:
            res = CCompiler(self.package).compile()
        if res and first_compiled is not {}:
            res = self.__do_compile(first_compiled)
            [all_files.pop(key) for key in first_compiled if first_compiled[key] == all_files[key]]
        if res:
            res = self.__do_compile(all_files)
        if res:
            self.__write_app_file(list(all_files.keys()))
        return res

    def common(self, log_dir: str) -> bool:
        info('common tests for ' + self.project_name)
        all_src = self.__get_all_files(self.test_path, 'erl')
        if self.__do_compile(all_src, output=self.test_path):
            return self.__do_common_test(log_dir)
        return False

    def unit(self) -> bool:  # TODO run unit tests only for modules with include eunit lib?
        info('unit tests for ' + self.project_name)
        debug('run eunit in ' + self.test_path)
        all_src = self.__get_all_files(self.test_path, 'erl')
        ensure_dir(self.output_path)
        if self.__do_compile(all_src, output=self.test_path):
            modules, test_dirs = self.__get_test_directories(all_src, '_SUITE')
            return self.__do_unit_test(modules, test_dirs)
        return False

    def __run_prebuild(self):
        for action in self.package.config.prebuild:
            action.run(self.root_path)

    @staticmethod  # TODO temporary static. See TODO below
    def form_compilation_order(files: dict) -> dict:
        first = {}
        for name, path in files.items():  # TODO make this check optional - based on package config.
            with open(join(path, name) + '.erl', 'r', encoding='utf-8') as f:
                parse_transform_first(first, files, f)
        return first

    def __do_compile(self, files: dict, output=None) -> bool:
        cmd = self.__compose_compiler_call(files, output)
        env_vars = self.__set_env_vars()
        return run_cmd(cmd, self.project_name, self.root_path, env_vars)

    def __do_unit_test(self, modules: list, test_dirs: list) -> bool:  # TODO make nice output and tests result sum
        cmd = self.__compose_unit_call(modules, test_dirs)
        return run_cmd(cmd, self.project_name, self.root_path, shell=True, output=True)

    def __do_common_test(self, log_dir: str) -> bool:
        cmd = self.__compose_ct_call(log_dir)
        return run_cmd(cmd, self.project_name, self.root_path, output=True)

    def __set_env_vars(self) -> dict:
        env_vars = dict(os.environ)
        if self.package.deps is not []:
            env_vars['ERL_LIBS'] = self.deps_path
        return env_vars

    def __compose_compiler_call(self, files: dict, output: str or None):
        cmd = [self.executable]
        if os.path.exists(self.include_path):
            cmd += ['-I', self.include_path]
        cmd += ['-pa', self.output_path]
        if output:
            cmd += ['-o', output]
        else:
            cmd += ['-o', self.output_path]
        self.__append_macro(cmd)
        for filename, path in files.items():
            cmd.append(join(path, filename) + '.erl')
        return cmd

    def __compose_unit_call(self, modules: list, test_dirs: list) -> str:
        cmd = 'erl '
        for test_dir in test_dirs:
            cmd += ' -pa ' + join('test', test_dir)
        cmd += ' -pa ' + self.output_path
        cmd += ' -pa ' + join(self.deps_path, '*/ebin')
        cmd += ' -eval "case eunit:test(' + str(modules) + ') of ok -> erlang:halt(0); _ -> erlang:halt(1) end"'
        return cmd

    def __compose_ct_call(self, log_dir: str) -> list:
        cmd = ['ct_run', '-no_auto_compile', '-noinput']
        cmd += ['-pa', self.output_path]
        cmd += ['-pa', join(self.deps_path, '*/ebin')]
        cmd += ['-pa', self.test_path]
        cmd += ['-dir', self.test_path]
        logs = join(self.root_path, log_dir)
        ensure_dir(logs)
        cmd += ['-logdir', logs]
        return cmd

    def __append_macro(self, cmd):
        for var in self.build_vars:
            if isinstance(var, dict):
                for k, v in var.items():
                    cmd += ['-D' + k + '=' + v]  # variable with value
            elif isinstance(var, str):
                cmd += ['-D' + var]  # just standalone variable

    def __write_app_file(self, all_files: list):
        if self.package.app_config.compose_app_file:  # TODO move me to app_config?
            app_src = read_file(join(self.src_path, self.project_name + '.app.src'))
            app_path = join(self.output_path, self.project_name + '.app')
            with open(app_path, 'w') as f:
                f.write(Template(app_src).render(modules=all_files, app=self.package, hostname=socket.gethostname()))

    # scan all folders, return dict, where module names are the keys, and their paths are the values
    def __get_all_files(self, path: str, extension: str) -> dict:
        return_files = {}
        extension_len = len(extension) + 1  # length of extension + dot
        for file in listdir(path):
            abs_file = join(path, file)
            if isdir(abs_file):
                subdir_files = self.__get_all_files(abs_file, extension)
                return_files.update(subdir_files)
            elif check_extension(abs_file, extension):
                module_name = file[:-extension_len]  # remove extension
                return_files[module_name] = path
        return return_files

    # get tests subdirectories from all_files directories, return as a unique list
    def __get_test_directories(self, all_files: dict, drop_extension: str) -> (list, list):
        to_link = set()
        modules = []
        for file, path in all_files.items():
            if not file.endswith(drop_extension):  # drop all common tests
                to_link.add(os.path.relpath(self.test_path, path))
                modules.append(file)
        return modules, list(to_link)
