import os
from os import listdir
from os.path import isfile, join, isdir
from subprocess import Popen, PIPE

from walrus.compiler import AbstractCompiler
from walrus.utils.file_utils import ensure_dir, read_file_lines, write_file_lines


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
        all_files = self.get_all_files(self.src_path, [])
        ensure_dir(self.output_path)
        return self.do_compile(all_files)

    def do_compile(self, files):
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
            return True

    # TODO change reading file by lines on decoding file on erlang terms, changing and encoding back
    def write_app_file(self, all_files):
        if self.compose_app_file:
            [app_src] = read_file_lines(join(self.src_path, self.project_name + '.app.src'))
            changed_file = []
            found = False
            (_, _, opts) = app_src
            for line in opts:
                if not found and line.strip().startswith('{modules'):
                    modified = append_modules_with_files(line, all_files)
                    found = True
                    changed_file.append(modified)
                else:
                    changed_file.append(line)
            if not found:
                create_modules_with_files(changed_file, all_files)
            write_file_lines(changed_file, join(self.output_path, self.project_name + '.app'))

    def get_all_files(self, path, files):
        all_files = listdir(path)
        for file in all_files:
            abs_file = join(path, file)
            if isdir(abs_file):
                self.get_all_files(abs_file, files)
            elif is_erlang_source(abs_file):
                files.append(abs_file)
        return files


def create_modules_with_files(lines, all_files):
    module_line = '{modules,' + str(all_files) + '},'
    lines.insert(len(lines) - 1, module_line)


def append_modules_with_files(line, all_files):
    [prefix, modules] = line.split('[')
    [modules, suffix] = modules.split(']')
    modules = modules.replace(']}', '')  # TODO if braces will be divided with space or endline this will not work
    modules_to_add = []
    modules_str = modules.split(',')
    for file in all_files:
        if file not in modules_str:
            modules_to_add.append(file)
    if not modules_to_add:
        return line
    else:
        return prefix + str(modules_str + modules_to_add) + suffix
