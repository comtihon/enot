import json
from os.path import join
from tarfile import TarFile

from enot.action import action_factory
from enot.action.release import Release
from enot.compiler.compiler_type import Compiler
from enot.packages.config.config import ConfigFile, get_dep_info_from_hex
from enot.packages.dep import Dep
from enot.utils.file_utils import read_file


def parse_deps(deps: list) -> dict:
    found = {}
    for dep in deps:
        name = dep['name']
        if 'url' not in dep:
            found[name] = get_dep_info_from_hex(name, dep['tag'])
        else:
            found[name] = Dep(dep['url'], dep.get('branch', None), tag=dep.get('tag', None))
    return found


class EnotConfig(ConfigFile):
    def __init__(self, config: dict, url=None, name=None):
        super().__init__()
        self._name = config.get('name', name)
        self._drop_unknown = config.get('drop_unknown_deps', True)
        self._with_source = config.get('with_source', True)
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
        self._auto_build_order = config.get('auto_build_order', True)
        self._override_conf = config.get('override', False)
        self._disable_prebuild = config.get('disable_prebuild', False)
        self._fullname = config.get('fullname', None)
        self._compare_versions = config.get('compare_versions', True)
        self._prebuild = EnotConfig.parse_steps(config.get('prebuild', []))
        self._install = EnotConfig.parse_steps(config.get('install', []))
        self._is_release = False
        for action in self.install:
            if isinstance(action, Release):
                self._is_release = True
        self._uninstall = EnotConfig.parse_steps(config.get('uninstall', []))

    @property
    def is_release(self) -> bool:
        return self._is_release

    @classmethod
    def from_path(cls, path: str, url=None) -> 'EnotConfig':
        content = read_file(join(path, 'enot_config.json'))
        name = path.split('/')[-1:]  # use project dir name as a name if not set in config
        return cls(json.loads(content), url=url, name=name)

    @classmethod
    def from_package(cls, package: TarFile, url: str, config: ConfigFile) -> 'EnotConfig':
        f = package.extractfile('enot_config.json')
        content = f.read()
        conf = cls(json.loads(content.decode('utf-8')), url=url)
        if config is not None:
            if config.fullname:  # overwrite fullname by package's fullname (from dep.config).
                conf.fullname = config.fullname
        return conf

    def need_enotify(self):
        return False

    def get_compiler(self):
        return Compiler.ENOT

    def __parse_build_vars(self, parsed):
        self._build_vars = parsed.get('build_vars', [])
        self._c_build_vars = parsed.get('c_build_vars', [])

    @staticmethod
    def parse_steps(steps: list) -> list:
        actions = []
        for step in steps:
            [(action_type, params)] = step.items()
            actions.append(action_factory.get_action(action_type, params))
        return actions
