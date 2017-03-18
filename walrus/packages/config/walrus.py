import json
from os.path import join

from walrus.action.prebuild import action_factory
from walrus.compiler.abstract import Compiler
from walrus.packages.config import ConfigFile
from walrus.utils.file_utils import read_file


class WalrusConfig(ConfigFile):
    def __init__(self, path, has_nif):
        super().__init__(path)
        self.path = path
        self.has_nifs = has_nif

    def read_config(self) -> dict:
        super().read_app_primary_params()
        content = read_file(join(self.path, 'walrusfile.json'))
        parsed = json.loads(content)
        self.name = parsed['name']
        if 'drop_unknown_deps' in parsed:
            self.drop_unknown = parsed['drop_unknown_deps']
        if 'with_source' in parsed:
            self.with_source = parsed['with_source']
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

    def parse_prebuild(self, parsed):
        if 'prebuild' in parsed:
            for action_type, params in parsed['prebuild'].items():
                self.prebuild.append(action_factory.get_action(action_type, params))
