import os
from os import listdir
from os.path import isfile, join, isdir
from subprocess import Popen, PIPE

from walrus.compiler import AbstractCompiler
from walrus.utils.file_utils import ensure_dir, read_file, write_file_lines


def is_erlang_source(file):
    return isfile(file) and file.split(".")[-1] == "erl"


class WalrusCompiler(AbstractCompiler):
    deps_path = ""
    compose_app_file = True

    def __init__(self, path: str, compose_app_file: bool, name: str):
        super().__init__()
        self.src_path = join(path, 'src')
        self.include_path = join(path, 'include')
        self.output_path = join(path, 'ebin')
        self.compose_app_file = compose_app_file
        self.deps_path = join(path, 'deps')
        self.project_name = name

    def compile(self) -> bool:
        print('build ' + self.project_name)
        filenames, all_files = self.get_all_files(self.src_path)
        ensure_dir(self.output_path)
        return self.do_compile(filenames, all_files)

    def do_compile(self, filenames, files):
        env_vars = dict(os.environ)
        cmd = [self.compiler, "-I", self.include_path, "-o", self.output_path]
        env_vars['ERL_LIBS'] = self.deps_path
        for file in files:
            cmd.append(file)
        p = Popen(cmd, stdout=PIPE, stderr=PIPE, env=env_vars)
        if p.wait() != 0:
            print("Compilation failed: ")
            err = p.stdout.read()
            print(err.decode('utf8'))
            return False
        else:
            self.write_app_file(filenames)
            return True

    # TODO change reading file by lines on decoding file on erlang terms, changing and encoding back
    def write_app_file(self, all_files):
        if self.compose_app_file:
            app_src = read_file(join(self.src_path, self.project_name + '.app.src'))
            if '{modules' in app_src:
                changed_file = append_modules_with_files(app_src, all_files)
            else:
                changed_file = create_modules_with_files(app_src, all_files)
            write_file_lines(changed_file, join(self.output_path, self.project_name + '.app'))

    def get_all_files(self, path):
        abs_files = []
        modules = []
        all_files = listdir(path)
        for file in all_files:
            abs_file = join(path, file)
            if isdir(abs_file):
                dir_file_names, dir_abs_files = self.get_all_files(abs_file)
                abs_files += dir_abs_files
                modules += dir_file_names
            elif is_erlang_source(abs_file):
                abs_files.append(abs_file)
                modules.append(file[:-4])  # remove .erl
        return modules, abs_files


def create_modules_with_files(app_src, all_files):
    [before, after] = str.split(app_src, '{applications,', 1)
    module_line = '{modules,' + str(all_files) + '},'
    return before + module_line + '{applications,' + after


def append_modules_with_files(app_src, all_files):
    [before, after] = str.split(app_src, '{modules,', 1)
    [modules, after_modules] = str.split(after, ']', 1)
    to_write = __get_modules_to_add(modules, all_files)
    print(to_write)
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
