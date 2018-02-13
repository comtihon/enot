from os.path import join

from erl_terms.erl_terms_core import decode

from enot.compiler.compiler_type import Compiler
from enot.packages.config.config import ConfigFile, get_dep_info_from_hex
from enot.packages.dep import Dep
from enot.utils.erl_file_utils import parse_platform_define
from enot.utils.file_utils import read_file


# TODO raw is not supported for now
def parse_dep_body(body: tuple) -> Dep:
    if body[0] != 'git':
        raise RuntimeError('Unsupported dep type: ' + body[0] + ', only git is supported.')
    if len(body) == 2:
        return Dep(body[1], 'master')
    if len(body) == 3:
        rev = body[2]
        if isinstance(rev, str):
            if rev == '':
                return Dep(body[1], 'HEAD')
            else:
                return Dep(body[1], rev)
        if len(rev) == 2 and rev[0] == 'branch' or rev[0] == 'ref':
            return Dep(body[1], rev[1])
        if len(rev) == 2 and rev[0] == 'tag':
            return Dep(body[1], 'master', rev[1])
    raise RuntimeError('Unknown dep ' + str(body))


class RebarConfig(ConfigFile):
    def __init__(self, path: str, url=None):
        super().__init__()
        self._path = path
        self._platform_defines = []
        rebarconfig = decode(read_file(join(path, 'rebar.config')))
        self._url = url
        self.__parse_config(rebarconfig)

    @property
    def platform_defines(self) -> list:
        return self._platform_defines

    def get_compiler(self):
        return Compiler.REBAR

    def __parse_config(self, config):
        for (key, value) in config:
            if key == 'deps':
                self.__parse_deps(value)
            if key == 'erl_opts':
                self.__parse_erl_opts(value)

    def __parse_deps(self, deps):
        for dep in deps:
            name = dep[0]
            if len(dep) == 2:  # {Dep, {git, Url, Rev}}
                body = dep[1]
            else:
                body = dep[2]  # {Dep, VsnRegex, {git, Url, Rev}}
            if isinstance(body, str):
                self.deps[name] = get_dep_info_from_hex(name, body)
            else:
                self.deps[name] = parse_dep_body(body)

    def __parse_erl_opts(self, value):
        for opt in value:
            if not isinstance(opt, str):
                if opt[0] == 'platform_define':
                    case, define = parse_platform_define(opt)
                    self.platform_defines.append({case, define})
                if opt[0] == 'd' and len(opt) == 2:  # opt[0] == d, opt[1] == {Var, Value}
                    self.build_vars.append(opt[1])
