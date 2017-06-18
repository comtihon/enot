import json
from os.path import join
from tarfile import TarFile

from coon.action.prebuild import action_factory
from coon.compiler.compiler_type import Compiler
from coon.packages.config.config import ConfigFile, get_dep_info_from_hex
from coon.packages.dep import Dep
from coon.utils.file_utils import read_file


def parse_deps(deps: list) -> dict:
    found = {}
    for dep in deps:
        name = dep['name']
        if 'url' not in dep:
            found[name] = get_dep_info_from_hex(name, dep['tag'])
        else:
            found[name] = Dep(dep['url'], dep.get('branch', None), tag=dep.get('tag', None))
    return found


class CoonConfig(ConfigFile):
    def __init__(self, config: dict, url=None, name=None):
        super().__init__()
        self._name = config.get('name', name)
        self._drop_unknown = config.get('drop_unknown_deps', True)
        self._with_source = config.get('with_source', True)
        self.__parse_prebuild(config)
        self.__parse_build_vars(config)
        self._deps = parse_deps(config.get('deps', {}))
        self._test_deps = parse_deps(config.get('test_deps', {}))
        self._conf_vsn = config.get('app_vsn', None)
        self._git_tag = config.get('tag', None)
        self._git_branch = config.get('branch', None)
        self._link_all = config.get('link_all', self.link_all)
        self._rescan_deps = config.get('rescan_deps', self.rescan_deps)
        self._url = config.get('url', url)
        self._erlang_versions = config.get('erlang', [])
        self._fullname = config.get('fullname', None)

    @classmethod
    def from_path(cls, path: str, url=None) -> 'CoonConfig':
        content = read_file(join(path, 'coonfig.json'))
        name = path.split('/')[-1:]  # use project dir name as a name if not set in config
        return cls(json.loads(content), url=url, name=name)

    @classmethod
    def from_package(cls, package: TarFile, url: str) -> 'CoonConfig':
        f = package.extractfile('coonfig.json')
        content = f.read()
        return cls(json.loads(content.decode('utf-8')), url=url)

    def need_coonsify(self):
        return False

    def get_compiler(self):
        return Compiler.COON

    def __parse_prebuild(self, parsed):
        for step in parsed.get('prebuild', []):
            [(action_type, params)] = step.items()
            self.prebuild.append(action_factory.get_action(action_type, params))

    def __parse_build_vars(self, parsed):
        self._build_vars = parsed.get('build_vars', [])
        self._c_build_vars = parsed.get('c_build_vars', [])
