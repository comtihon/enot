from os import listdir
from os.path import isfile, join, isdir
from subprocess import Popen, PIPE

from walrus.packages.config.config import ConfigFile
from walrus.utils.file_utils import ensure_dir
from walrus.compiler import AbstractCompiler


def is_erlang_source(file):
    return isfile(file) and file.split(".")[-1] == "erl"


class WalrusCompiler(AbstractCompiler):
    deps = []
    deps_path = ""

    def __init__(self, package_config: ConfigFile):
        super().__init__()
        self.src_path = join(package_config.path, 'src')
        self.include_path = join(package_config.path, 'include')
        self.output_path = join(package_config.path, 'ebin')
        self.deps_path = join(package_config.path, 'deps')
        print(self.deps_path)
        self.deps = [dep.name for dep in package_config.deps]

    def compile(self):
        all_files = self.get_all_files(self.src_path, [])
        all_deps = self.get_all_deps()
        print(all_deps)
        ensure_dir(self.output_path)
        return self.do_compile(all_files, all_deps)

    # TODO use ERL_LIBS=deps instead? (think about parse-transform)
    # TODO add dep names to app.src modules and copy app.src to disk
    def do_compile(self, files, deps):
        cmd = [self.compiler, "-I", self.include_path, "-o", self.output_path]
        if deps:
            for dep in deps:
                cmd.append("-pa")
                cmd.append(dep)
        for file in files:
            cmd.append(file)
        print('args ' + str(cmd))
        p = Popen(cmd, stdout=PIPE, stderr=PIPE)
        if p.wait() != 0:
            print("Compilation failed: ")
            err = p.stdout.read()
            print(err.decode('utf8'))
            return False
        else:
            return True

    def get_all_files(self, path, files):
        all_files = listdir(path)
        for file in all_files:
            abs_file = join(path, file)
            if isdir(abs_file):
                self.get_all_files(abs_file, files)
            elif is_erlang_source(abs_file):
                files.append(abs_file)
        return files

    def get_all_deps(self):
        return [join(self.deps_path, dep) for dep in self.deps]
