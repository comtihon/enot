import json
from os.path import join

from walrus.packages.config import ConfigFile
from walrus.packages.dep import Dep
from walrus.utils.file_utils import read_file
from walrus.compiler.abstract import Compiler


class WalrusConfig(ConfigFile):
    def __init__(self, path):
        super().__init__(path)
        self.path = path
        self.read_config()

    def read_config(self):
        content = read_file(join(self.path, 'walrusfile.json'))
        parsed = json.loads(content)
        self.name = parsed['name']
        self.deps = [Dep.fromjson(x) for x in parsed['deps']]
        self.drop_unknown = parsed['drop_unknown_deps']
        self.with_source = parsed['with_source']

    def need_walrusify(self):
        return False

    def get_compiler(self):
        return Compiler.WALRUS
