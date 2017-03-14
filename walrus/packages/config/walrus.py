import json
from os.path import join

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
        self.drop_unknown = parsed['drop_unknown_deps']
        self.with_source = parsed['with_source']
        return self.parse_deps(parsed['deps'])

    def need_walrusify(self):
        return False

    def get_compiler(self):
        return Compiler.WALRUS

    def parse_deps(self, deps):
        return_deps = {}
        for dep in deps:
            name = dep['name']
            if name in self.app_deps:
                return_deps[name] = (dep['url'], dep['vsn'])
            else:
                print('Drop unused dep ' + name)
        return return_deps
