import json
from os.path import join

from walrus.action.prebuild import action_factory
from walrus.compiler.abstract import Compiler
from walrus.packages.config import ConfigFile
from walrus.utils.file_utils import read_file


class WalrusConfig(ConfigFile):
    def __init__(self, path, has_nif=False):
        super().__init__(path)
        self._path = path
        self._has_nifs = has_nif

    def read_config(self) -> dict:
        super().read_app_primary_params()
        content = read_file(join(self.path, 'walrusfile.json'))
        return self.init_from_json(content)

    def init_from_json(self, content: str):
        parsed = json.loads(content)
        self._name = parsed['name']
        if 'drop_unknown_deps' in parsed:
            self._drop_unknown = parsed['drop_unknown_deps']
        if 'with_source' in parsed:
            self._with_source = parsed['with_source']
        if 'version' in parsed:
            self._conf_vsn = parsed['version']
        self.__parse_prebuild(parsed)
        self.__parse_build_vars(parsed)
        return self.__parse_deps(parsed['deps'])

    def need_walrusify(self):
        return False

    def get_compiler(self):
        return Compiler.WALRUS

    def __parse_deps(self, deps):
        return_deps = {}
        for dep in deps:
            name = dep['name']
            if name in self.app_deps:
                return_deps[name] = (dep['url'], dep['vsn'])
            else:
                print('Drop unused dep ' + name)
        return return_deps

    def __parse_prebuild(self, parsed):
        if 'prebuild' in parsed:
            for step in parsed['prebuild']:
                [(action_type, params)] = step.items()
                self.prebuild.append(action_factory.get_action(action_type, params))

    def __parse_build_vars(self, parsed):
        if 'build_vars' in parsed:
            for keyvalue in parsed['build_vars'].items():
                self.build_vars.append(keyvalue)
