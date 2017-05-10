import re
from distutils.sysconfig import parse_makefile
from os.path import join

from coon.compiler.compiler_type import Compiler
from coon.packages.config.config import ConfigFile
from coon.utils.file_utils import read_file_lines


def get_erl_opts(args: list, content: dict) -> list:
    return_vars = []
    for arg in args:
        if arg.startswith('-D'):
            var = arg[2:]
            if '=' in var:
                [k, v] = var.split('=')
                k = check_var(k, content)
                v = check_var(v, content)
                return_vars.append((k, v))
            else:
                return_vars.append(check_var(var, content))
    return return_vars


def check_var(var: str, content: dict):
    if var.startswith('$'):
        match = re.search('\$\((.*)\)', var)
        if match:
            return content[match.groups()[0]]
        else:
            return content[var[1:]]
    else:
        return var


def get_dep(line):
    [_, url, tag] = line.strip().split(" ")
    return url, tag


class ErlangMkConfig(ConfigFile):
    def __init__(self, path, has_nif):
        super().__init__(path)
        self._path = path
        self._has_nifs = has_nif

    def read_config(self) -> dict:
        super().read_app_primary_params()
        makefile = join(self.path, 'Makefile')
        content = parse_makefile(makefile)
        self.__conf_init(content)
        self.__parse_erl_opts(makefile, content)
        return self.__parse_deps(content)

    def get_compiler(self):
        return Compiler.ERLANG_MK

    def __parse_deps(self, content: dict):
        return_deps = {}
        if 'DEPS' in content:
            deps = content['DEPS'].split(' ')
            for dep in deps:
                depname = 'dep_' + dep
                if depname in content:
                    url, tag = get_dep(content[depname])
                    if dep in deps and dep in self.applications:
                        return_deps[dep] = (url, tag)
                    else:  # TODO should we drop unused here? (same as coon.__parse_deps/2)
                        print('Drop unused dep ' + dep)
                else:
                    print('Dep ' + depname + ' not specified')
        return return_deps

    def __conf_init(self, content: dict):
        self.__conf_vsn = content.get('PROJECT_VERSION', None)

    def __parse_erl_opts(self, mkfile_path: str, content: dict):

        if 'ERLC_OPTS' in content:
            opt_str = content['ERLC_OPTS']
            self._build_vars = get_erl_opts(opt_str.split(' '), content)
        else:  # no ERLC_OPTS in Makefile. Should scan it manually (+= can be used instead of = )
            data = read_file_lines(mkfile_path)
            lines = [x.strip('\n') for x in data]
            for line in lines:
                if line.startswith('ERLC_OPTS'):
                    opt_str = line.split(' ')[2:]
                    self._build_vars = get_erl_opts(opt_str, content)
                    return
