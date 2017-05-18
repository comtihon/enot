import subprocess
from os.path import isfile, join, isdir
from subprocess import PIPE

import os
from jinja2 import Template
from os import listdir

from coon.compiler.abstract import AbstractCompiler
from coon.compiler.c_compiler import CCompiler
from coon.utils.file_utils import ensure_dir, read_file


def is_erlang_source(file):
    return isfile(file) and file.split(".")[-1] == "erl"


class CoonCompiler(AbstractCompiler):
    @property
    def deps_path(self) -> str:
        return join(self.package.path, 'deps')

    def compile(self) -> bool:
        print('build ' + self.project_name)
        self.__run_prebuild()
        filenames, all_files = self.__get_all_files(self.src_path)
        print('ensure ' + self.output_path)
        ensure_dir(self.output_path)
        if self.package.has_nifs:
            if CCompiler(self.package).compile():
                return self.__do_compile(filenames, all_files)
        else:
            return self.__do_compile(filenames, all_files)
        return False

    def __run_prebuild(self):
        for action in self.package.config.prebuild:
            action.run(self.root_path)

    def __do_compile(self, filenames, files):
        cmd = self.__compose_compiler_call(files)
        env_vars = self.__set_env_vars()
        p = subprocess.Popen(cmd, stdout=PIPE, stderr=PIPE, env=env_vars)
        if p.wait() != 0:
            print("Compilation failed: ")
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            self.__write_app_file(filenames)
            return True

    def __set_env_vars(self):
        env_vars = dict(os.environ)
        if self.package.deps is not {}:
            env_vars['ERL_LIBS'] = self.deps_path
        return env_vars

    def __compose_compiler_call(self, files):
        cmd = [self.executable]
        if os.path.exists(self.include_path):
            cmd += ['-I', self.include_path]
        cmd += ['-o', self.output_path]
        self.__append_macro(cmd)
        for file in files:
            cmd.append(file)
        return cmd

    def __append_macro(self, cmd):
        for var in self.build_vars:
            if isinstance(var, dict):
                for k, v in var.items():
                    cmd += ['-D' + k + '=' + v]  # variable with value
            elif isinstance(var, str):
                cmd += ['-D' + var]  # just standalone variable

    def __write_app_file(self, all_files):
        if self.package.app_config.compose_app_file:  # TODO move me to app_config?
            app_src = read_file(join(self.src_path, self.project_name + '.app.src'))
            app_path = join(self.output_path, self.project_name + '.app')
            with open(app_path, 'w') as f:
                f.write(Template(app_src).render(modules=all_files, app=self.package))

    def __get_all_files(self, path):
        abs_files = []
        modules = []
        all_files = listdir(path)
        for file in all_files:
            abs_file = join(path, file)
            if isdir(abs_file):
                dir_file_names, dir_abs_files = self.__get_all_files(abs_file)
                abs_files += dir_abs_files
                modules += dir_file_names
            elif is_erlang_source(abs_file):
                abs_files.append(abs_file)
                modules.append(file[:-4])  # remove .erl
        return modules, abs_files
