import json
import os
from os.path import join

from walrus.compiler import Compiler
from walrus.pac_cache import cache_factory
from walrus.utils.file_utils import write_file, read_file


class WalrusGlobalProperties:
    cache_url = ""
    temp_dir = ""
    compiler = ""  # walrus | rebar | erlang.mk | rebar3 | package-local
    cache = None

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
        self.set_compiler(conf)
        self.cache = cache_factory.get_cache(self.temp_dir, self.cache_url)

    @staticmethod
    def get_default_conf():
        return \
            {
                'cache_url': 'file:///home/tihon/.walrus/cache',
                'temp_dir': '/tmp/walrus',
                'compiler': 'walrus'
            }

    def set_compiler(self, conf):
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
