import json
from os.path import join

from appdirs import *
from pkg_resources import Requirement, resource_filename

import walrus
from walrus.compiler import Compiler
from walrus.pac_cache.cache_man import CacheMan
from walrus.utils.file_utils import read_file, ensure_dir


def init_config(source, path, file):
    cache_dir = user_cache_dir(walrus.APPNAME, walrus.APPAUTHOR)
    ensure_dir(path)
    ensure_dir(cache_dir)
    with open(source, 'r') as r:
        content = r.read()
    with open(join(path, file), 'w') as f:
        f.write(str.replace(content, '{{ local_cache }}', cache_dir))


class WalrusGlobalProperties:
    temp_dir = ""
    compiler = ""  # walrus | rebar | erlang.mk | rebar3 | package-local
    cache: CacheMan = None

    def __init__(self, path=user_config_dir(walrus.APPNAME)):
        if not os.path.exists(path):
            os.makedirs(path)
        config_path = join(path, 'global_config.json')
        if not os.path.exists(config_path):
            template = resource_filename(Requirement.parse(walrus.APPNAME), 'walrus/resources/global_config.json')
            init_config(template, path, 'global_config.json')
        content = read_file(config_path)
        conf = json.loads(content)
        self.temp_dir = conf['temp_dir']
        self.__set_compiler(conf)
        self.__set_up_cache(conf)

    def __set_up_cache(self, conf: dict):
        self.cache = CacheMan(conf)

    def __set_compiler(self, conf):
        if conf['compiler'] == 'walrus':
            self.compiler = Compiler.WALRUS
        elif conf['compiler'] == 'rebar':
            self.compiler = Compiler.REBAR
        elif conf['compiler'] == 'erlang.mk':
            self.compiler = Compiler.ERLANG_MK
        elif conf['compiler'] == 'package-local':
            self.compiler = Compiler.LOCAL
        else:
            print('Unknown complier : ' + conf['compiler'] + ' will use walrus')
            self.compiler = Compiler.WALRUS