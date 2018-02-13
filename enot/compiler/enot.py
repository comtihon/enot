import os
import socket
from os import listdir
from os.path import isfile, join, isdir

from jinja2 import Template

from enot.compiler.abstract import AbstractCompiler, run_cmd
from enot.compiler.c_compiler import CCompiler
from enot.pac_cache import Static
from enot.packages.config.config import ConfigFile
from enot.utils.file_utils import ensure_dir, read_file
from enot.utils.logger import debug, info


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


class EnotCompiler(AbstractCompiler):
    def __init__(self, package, define: str = '', executable='erlc'):
        super().__init__(package, executable)
        self._define = define

    @property
    def define(self) -> list:
        defines = []
        for define in self._define.split(' '):
            if define != '' and define != "''":
                defines.append(['-D', define])
        return defines

    @property
    def deps_path(self) -> str:
        return join(self.package.path, 'deps')

    def compile(self, override_config: ConfigFile or None = None) -> bool:
        info('Enot build ' + self.project_name)
        self.__run_prebuild(override_config)
        all_files = self.__get_all_files(self.src_path, 'erl')
        first_compiled = self.form_compilation_order(all_files)
        debug('ensure ' + self.output_path)
        ensure_dir(self.output_path)
        res = True
        if self.package.has_nifs:
            res = CCompiler(self.package).compile(override_config=override_config)
        if res and first_compiled is not {}:
            res = self.__do_compile(first_compiled, override=override_config)
            [all_files.pop(key) for key in first_compiled if first_compiled[key] == all_files[key]]
        if res:
            res = self.__do_compile(all_files, override=override_config)
        if res:
            self.__write_app_file(list(all_files.keys()))
        return res

    def common(self, log_dir: str) -> bool:  # TODO should I add override config compilation for tests?
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
            modules, test_dirs = self.__get_test_directories(all_src, drop_extension='_SUITE')
            return self.__do_unit_test(modules, test_dirs)
        return False

    # run prebuild if it is not disabled in package's config,
    # or disabled in root config with override set to True
    def __run_prebuild(self, config: ConfigFile or None):
        if not self.package.config.disable_prebuild and \
                not (config is not None and config.override_conf and config.disable_prebuild):
            for action in self.package.config.prebuild:
                action.run(self.root_path)

    def form_compilation_order(self, files: dict) -> dict:
        if not self.package.config.auto_build_order:  # source analysis disabled
            return {}
        first = {}
        for name, path in files.items():
            with open(join(path, name) + '.erl', 'r', encoding='utf-8') as f:
                parse_transform_first(first, files, f)
        return first

    def __do_compile(self, files: dict, override: ConfigFile or None = None, output=None) -> bool:
        cmd = self.__compose_compiler_call(files, output, override)
        env_vars = self.__set_env_vars()
        return run_cmd(cmd, self.project_name, self.root_path, env_vars)

    def __do_unit_test(self, modules: list, test_dirs: list) -> bool:  # TODO make nice output and tests result sum
        cmd = self.__compose_unit_call(modules, test_dirs)
        return run_cmd(cmd, self.project_name, self.root_path, shell=True, output=None)

    def __do_common_test(self, log_dir: str) -> bool:
        cmd = self.__compose_ct_call(log_dir)
        env_vars = self.__set_env_vars()
        return run_cmd(cmd, self.project_name, self.root_path, env_vars, output=None)

    def __set_env_vars(self) -> dict:
        env_vars = dict(os.environ)
        if self.package.deps is not []:
            env_vars['ERL_LIBS'] = self.deps_path
        return env_vars

    def __compose_compiler_call(self, files: dict, output: str or None, override):
        cmd = [self.executable]
        if os.path.exists(self.include_path):
            cmd += ['-I', self.include_path]
        cmd += ['-pa', self.output_path]
        if output:
            cmd += ['-o', output]
        else:
            cmd += ['-o', self.output_path]
        defines = self.define
        for define in defines:
            cmd += define
        self.__append_macro(cmd, override)
        for filename, path in files.items():
            cmd.append(join(path, filename) + '.erl')
        return cmd

    def __compose_unit_call(self, modules: list, test_dirs: list) -> str:
        cmd = 'erl'
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

    def __append_macro(self, cmd, override: ConfigFile or None):
        if override is not None and override.override_conf:
            build_vars = override.build_vars
        else:
            build_vars = self.build_vars
        for var in build_vars:
            if isinstance(var, dict):
                for k, v in var.items():
                    cmd += ['-D' + k + '=' + v]  # variable with value
            elif isinstance(var, str):
                cmd += ['-D' + var]  # just standalone variable

    def __write_app_file(self, all_files: list):
        if self.package.app_config.compose_app_file:
            app_src = read_file(join(self.src_path, self.project_name + '.app.src'))
            app_path = join(self.output_path, self.project_name + '.app')
            params = {x: os.environ[x] for x in os.environ}
            params['modules'] = all_files
            params['app'] = self.package
            params['hostname'] = socket.gethostname()
            params['erl'] = Static.get_erlang_version()
            with open(app_path, 'w') as f:
                f.write(Template(app_src).render(params))

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
                to_link.add(os.path.relpath(path, self.test_path))
                modules.append(file)
        return modules, list(to_link)
