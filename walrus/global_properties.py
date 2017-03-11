import json
import os
from os.path import join

from walrus.compiler import Compiler
from walrus.pac_cache import CacheType
from walrus.pac_cache.cache_man import CacheMan
from walrus.utils.file_utils import write_file, read_file


class WalrusGlobalProperties:
    cache_url = ""
    temp_dir = ""
    compiler = ""  # walrus | rebar | erlang.mk | rebar3 | package-local
    cache: CacheMan = None

    def __init__(self, path='/home/tihon/.walrus'):  # TODO hardcoded config    #TODO make os independent
        if not os.path.exists(path):
            os.makedirs(path)
        config_path = join(path, 'config.json')
        if not os.path.exists(config_path):
            conf = WalrusGlobalProperties.get_default_conf()
            write_file(config_path, json.dumps(conf))  # TODO handle write error
        else:
            content = read_file(config_path)
            conf = json.loads(content)
        self.cache_url = conf['cache_url']
        self.temp_dir = conf['temp_dir']
        self.__set_compiler(conf)
        self.__set_up_cache(conf)

    @staticmethod
    def get_default_conf():  # TODO remove me to resource
        return \
            {
                'compiler': 'walrus',
                'cache':
                    [
                        {
                            'type': CacheType.LOCAL,
                            'url': 'file:///home/tihon/.walrus/cache',
                            'temp_dir': '/tmp/walrus',
                        },
                        {  # TODO remove this test entry
                            'type': CacheType.ARTIFACTORY,
                            'url': 'http://127.0.0.1:8081/artifactory/generic-local'
                        }
                    ]
            }

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
