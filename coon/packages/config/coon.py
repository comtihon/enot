import json
from os.path import join
import mock

from coon.action.prebuild import action_factory
from coon.compiler.compiler_type import Compiler
from coon.packages.config.config import ConfigFile
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

    def init_from_json(self, content: str) -> dict:
        parsed = json.loads(content)
        return self.init_from_dict(parsed)

    def init_from_dict(self, content: dict) -> dict:
        self._name = content['name']
        self._drop_unknown = content.get('drop_unknown_deps', True)
        self._with_source = content.get('with_source', True)
        self._conf_vsn = content.get('version', None)
        self._has_nifs = content.get('has_nifs', self._has_nifs)  # TODO Get rid of this.
        self.__parse_prebuild(content)
        self.__parse_build_vars(content)
        return self.__parse_deps(content.get('deps', []))

    def need_coonsify(self):
        return False

    def get_compiler(self):
        return Compiler.COON

    # TODO refactor me. There can be a case when applications is empty and should be populated with deps
    def __parse_deps(self, deps) -> dict:
        return_deps = {}
        for dep in deps:
            name = dep['name']
            if name not in self.applications and self.applications is not []:  # If applications were modified by hand
                print('Unused dep ' + name)  # Warn user about unused deps
            return_deps[name] = (dep['url'], dep['vsn'])
        return return_deps

    def __parse_prebuild(self, parsed):
        for step in parsed.get('prebuild', []):
            [(action_type, params)] = step.items()
            self.prebuild.append(action_factory.get_action(action_type, params))

    def __parse_build_vars(self, parsed):
        self._build_vars = parsed.get('build_vars', [])
        self._c_build_vars = parsed.get('c_build_vars', [])
