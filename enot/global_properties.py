import json
from os.path import join

import enot
from appdirs import *
from jinja2 import Template
from pkg_resources import Requirement, resource_filename

from enot.compiler.compiler_type import Compiler
from enot.pac_cache.cache_man import CacheMan
from enot.utils.file_utils import read_file, ensure_dir
from enot.utils.logger import info


def temp_dir() -> str:  # TODO get system temp dir (os independent)
    return '/tmp/enot'


def init_config(source: str, path: str, file: str):
    cache_dir = user_cache_dir(enot.APPNAME, enot.APPAUTHOR)
    ensure_dir(path)
    ensure_dir(cache_dir)
    with open(source, 'r') as r:
        content = r.read()
    with open(join(path, file), 'w') as f:
        f.write(Template(content).render(local_cache=cache_dir, temp_dir=temp_dir()))


def ensure_conf_file(path: str) -> str:
    if not os.path.exists(path):
        os.makedirs(path)
    config_path = join(path, 'global_config.json')
    if not os.path.exists(config_path):
        template = resource_filename(Requirement.parse(enot.APPNAME), 'enot/resources/global_config.json')
        init_config(template, path, 'global_config.json')
    return config_path


class GlobalProperties:
    def __init__(self, path=user_config_dir(enot.APPNAME)):
        config_path = ensure_conf_file(path)
        content = read_file(config_path)
        conf = json.loads(content)
        self._conf_dir = path
        self.__init_from_dict(conf)

    @property
    def temp_dir(self) -> str:
        return self._temp_dir

    @property
    def conf_dir(self) -> str:
        return self._conf_dir

    @property
    def compiler(self) -> Compiler:
        return self._compiler

    @property
    def cache(self) -> CacheMan:
        return self._cache

    def __init_from_dict(self, conf: dict):
        self._temp_dir = conf['temp_dir']
        self.__set_compiler(conf)
        self._cache = CacheMan(conf)

    def __set_compiler(self, conf):
        try:
            self._compiler = Compiler(conf.get('compiler', 'enot'))
        except ValueError:
            info('Unknown complier : ' + conf['compiler'] + ' will use ' + Compiler.ENOT.value)
            self._compiler = Compiler.ENOT
