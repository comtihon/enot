import json
from os.path import join

import coon
from appdirs import *
from jinja2 import Template
from pkg_resources import Requirement, resource_filename

from coon.compiler.compiler_type import Compiler
from coon.pac_cache.cache_man import CacheMan
from coon.utils.file_utils import read_file, ensure_dir


def init_config(source, path, file):
    cache_dir = user_cache_dir(coon.APPNAME, coon.APPAUTHOR)
    ensure_dir(path)
    ensure_dir(cache_dir)
    with open(source, 'r') as r:
        content = r.read()
    with open(join(path, file), 'w') as f:
        f.write(Template(content).render(local_cache=cache_dir, temp_dir='/tmp/coon'))


class GlobalProperties:
    def __init__(self, path=user_config_dir(coon.APPNAME)):
        if not os.path.exists(path):
            os.makedirs(path)
        config_path = join(path, 'global_config.json')
        if not os.path.exists(config_path):
            template = resource_filename(Requirement.parse(coon.APPNAME), 'coon/resources/global_config.json')
            init_config(template, path, 'global_config.json')
        content = read_file(config_path)
        conf = json.loads(content)
        self._temp_dir = conf['temp_dir']
        self.__set_compiler(conf)
        self._cache = CacheMan(conf)

    @property
    def temp_dir(self) -> str:
        return self._temp_dir

    @property
    def compiler(self) -> Compiler:
        return self._compiler

    @property
    def cache(self) -> CacheMan:
        return self._cache

    def __set_compiler(self, conf):
        try:
            self._compiler = Compiler(conf.get('compiler', 'coon'))
        except ValueError:
            print('Unknown complier : ' + conf['compiler'] + ' will use ' + Compiler.COON.value)
            self._compiler = Compiler.COON
