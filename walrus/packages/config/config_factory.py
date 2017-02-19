from os import listdir
from os.path import isfile, join

from walrus.packages.config import ErlangMkConfig, WalrusConfig, RebarConfig, ConfigFile


def read_project(path):
    files = [f for f in listdir(path) if isfile(join(path, f))]
    if 'walrusfile.json' in files:
        return WalrusConfig(path)
    elif 'erlang.mk' in files:
        return ErlangMkConfig(path)
    elif 'rebar.config' in files:
        return RebarConfig(path)
    raise ValueError("Unknown build system in project " + path)


# read config based on path, merge with stub config
def upgrade_conf(path, conf: ConfigFile):
    dep_conf = read_project(path)
    dep_conf.read_app_primary_params()
    if dep_conf.name != conf.name:
        print('wrong name specified ' + dep_conf.name + ' vs ' + conf.name)
        dep_conf.name = conf.name
    return dep_conf
