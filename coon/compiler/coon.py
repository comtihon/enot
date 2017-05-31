import socket
import subprocess
from os import listdir
from os.path import isfile, join, isdir
from subprocess import PIPE

import os
from jinja2 import Template

from coon.compiler.abstract import AbstractCompiler
from coon.compiler.c_compiler import CCompiler
from coon.utils.file_utils import ensure_dir, read_file


def is_erlang_source(file):
    return isfile(file) and file.split(".")[-1] == "erl"


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
        print('build ' + self.project_name)
        self.__run_prebuild()
        all_files = self.__get_all_files(self.src_path)
        first_compiled = CoonCompiler.form_compilation_order(all_files)
        print('ensure ' + self.output_path)
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

    def __run_prebuild(self):
        for action in self.package.config.prebuild:
            action.run(self.root_path)

    @staticmethod  # TODO temporary static. See TODO below
    def form_compilation_order(files: dict) -> dict:
        first = {}
        for name, path in files.items():  # TODO make this check optional - based on package config.
            with open(join(path, name) + '.erl', 'r') as f:
                parse_transform_first(first, files, f)
        return first

    def __do_compile(self, files: dict):
        cmd = self.__compose_compiler_call(files)
        env_vars = self.__set_env_vars()
        p = subprocess.Popen(cmd, stdout=PIPE, stderr=PIPE, env=env_vars)
        if p.wait() != 0:
            print("Compilation failed: ")
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            return True

    def __set_env_vars(self):
        env_vars = dict(os.environ)
        if self.package.deps is not []:
            env_vars['ERL_LIBS'] = self.deps_path
        return env_vars

    def __compose_compiler_call(self, files: dict):
        cmd = [self.executable]
        if os.path.exists(self.include_path):
            cmd += ['-I', self.include_path]
        cmd += ['-pa', self.output_path]
        cmd += ['-o', self.output_path]
        self.__append_macro(cmd)
        for filename, path in files.items():
            cmd.append(join(path, filename) + '.erl')
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
    def __get_all_files(self, path: str) -> dict:
        return_files = {}
        for file in listdir(path):
            abs_file = join(path, file)
            if isdir(abs_file):
                subdir_files = self.__get_all_files(abs_file)
                return_files.update(subdir_files)
            elif is_erlang_source(abs_file):
                module_name = file[:-4]  # remove .erl
                return_files[module_name] = path
        return return_files
