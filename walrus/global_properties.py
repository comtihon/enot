import json
import os
import shlex
import subprocess
from os.path import join

from walrus.utils.file_utils import write_file, read_file
from walrus.compiler import Compiler


class WalrusGlobalProperties:
    cache_url = ""
    temp_dir = ""
    compiler = ""  # walrus | rebar | erlang.mk | rebar3 | package-local

    erlang_version = ""

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
        self.erlang_version = WalrusGlobalProperties.get_erlang_version()

    @staticmethod
    def get_default_conf():
        return \
            {
                'cache_url': 'file:///home/tihon/.walrus/cache',
                'temp_dir': '/tmp/walrus',
                'compiler': 'walrus'
            }

    @staticmethod
    def get_erlang_version():
        proc = subprocess.run(
            shlex.split("erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell"),
            stdout=subprocess.PIPE)
        if proc.returncode == 0:
            version = proc.stdout.decode('utf-8').strip()
            return version.translate({ord(c): None for c in '"'})
        else:
            print("Can't get erlang version")  # TODO handle error
            return None

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
