import json
from os.path import join

from coon.action.prebuild import action_factory
from coon.compiler.abstract import Compiler
from coon.packages.config import ConfigFile
from coon.utils.file_utils import read_file


class CoonConfig(ConfigFile):
    def __init__(self, path, has_nif=False):
        super().__init__(path)
        self._path = path
        self._has_nifs = has_nif

    def read_config(self) -> dict:
        super().read_app_primary_params()
        content = read_file(join(self.path, 'coonfig.json'))
        return self.init_from_json(content)

    def init_from_json(self, content: str):
        parsed = json.loads(content)
        self._name = parsed['name']
        self._drop_unknown = parsed.get('drop_unknown_deps', True)
        self._with_source = parsed.get('with_source', True)
        self._conf_vsn = parsed.get('version', None)
        self._has_nifs = parsed.get('has_nifs', self._has_nifs)  # TODO Get rid of this.
        self.__parse_prebuild(parsed)
        self.__parse_build_vars(parsed)
        return self.__parse_deps(parsed['deps'])

    def need_coonsify(self):
        return False

    def get_compiler(self):
        return Compiler.COON

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
        for step in parsed.get('prebuild', []):
            [(action_type, params)] = step.items()
            self.prebuild.append(action_factory.get_action(action_type, params))

    def __parse_build_vars(self, parsed):
        self._build_vars = parsed.get('build_vars', [])
        self._c_build_vars = parsed.get('c_build_vars', [])
