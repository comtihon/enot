import subprocess
from os.path import isfile, join, isdir
from subprocess import PIPE

import os
from os import listdir
from walrus.compiler import AbstractCompiler

from walrus.compiler.c_compiler import CCompiler
from walrus.utils.file_utils import ensure_dir, read_file, write_file_lines


def is_erlang_source(file):
    return isfile(file) and file.split(".")[-1] == "erl"


class WalrusCompiler(AbstractCompiler):
    @property
    def deps_path(self) -> str:
        return join(self.config.path, 'deps')

    def compile(self) -> bool:
        print('build ' + self.project_name)
        self.__run_prebuild()
        filenames, all_files = self.__get_all_files(self.src_path)
        ensure_dir(self.output_path)
        if self.config.has_nifs:
            if CCompiler(self.config).compile():
                return self.__do_compile(filenames, all_files)
        return False

    def __run_prebuild(self):
        for action in self.config.prebuild:
            action.run(self.root_path)

    def __do_compile(self, filenames, files):
        env_vars = dict(os.environ)
        cmd = self.__compose_compiler_call(env_vars, files)
        p = subprocess.Popen(cmd, stdout=PIPE, stderr=PIPE, env=env_vars)
        if p.wait() != 0:
            print("Compilation failed: ")
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            self.__write_app_file(filenames)
            return True

    def __compose_compiler_call(self, env_vars, files):
        cmd = [self.compiler, "-I", self.include_path, "-o", self.output_path]
        self.__append_macro(cmd)
        env_vars['ERL_LIBS'] = self.deps_path
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

    # TODO change reading file by lines on decoding file on erlang terms, changing and encoding back
    # TODO append conf version if app version is not set
    def __write_app_file(self, all_files):
        if self.config.compose_app_file:
            app_src = read_file(join(self.src_path, self.project_name + '.app.src'))
            if '{modules' in app_src:
                changed_file = append_modules_with_files(app_src, all_files)
            else:
                changed_file = create_modules_with_files(app_src, all_files)
            write_file_lines(changed_file, join(self.output_path, self.project_name + '.app'))

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


# TODO can use j2 template instead
def create_modules_with_files(app_src, all_files):
    [before, after] = str.split(app_src, '{applications,', 1)
    module_line = '{modules,' + str(all_files) + '},'
    return before + module_line + '{applications,' + after


def append_modules_with_files(app_src, all_files):
    [before, after] = str.split(app_src, '{modules,', 1)
    [modules, after_modules] = str.split(after, ']', 1)
    to_write = __get_modules_to_add(modules, all_files)
    if not to_write:
        return app_src
    else:
        return before + '{modules,' + str(to_write) + after_modules


def __get_all_existing_modules(all_modules_str):
    splitted = str.split(all_modules_str, ',')
    existing = [x.strip("\n[ ") for x in splitted]
    if existing == ['']:
        return []
    else:
        return existing


def __get_modules_to_add(all_modules, all_files):
    existing_modules = __get_all_existing_modules(all_modules)
    modules_to_add = []
    for file in all_files:
        if file not in existing_modules:
            modules_to_add.append(file)
    if not modules_to_add:
        return []
    else:
        return modules_to_add + existing_modules
