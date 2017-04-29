from os import listdir
from os.path import isfile, join

from coon.packages.config import ErlangMkConfig, CoonConfig, RebarConfig, ConfigFile


def read_project(path):
    files, dirs = split_files_and_dirs(path)
    has_nif = 'c_src' in dirs
    if 'coonfig.json' in files:
        return CoonConfig(path, has_nif)
    elif 'erlang.mk' in files:
        return ErlangMkConfig(path, has_nif)
    elif 'rebar.config' in files:
        return RebarConfig(path, has_nif)
    raise ValueError("Unknown build system in project " + path)


# read config based on path, merge with stub config
def upgrade_conf(path, conf: ConfigFile):
    dep_conf = read_project(path)
    dep_conf.read_app_primary_params()
    if dep_conf.name != conf.name:
        print('wrong name specified ' + dep_conf.name + ' vs ' + conf.name)
        dep_conf.name = conf.name
    return dep_conf


def split_files_and_dirs(path):
    all_objects = listdir(path)
    files = []
    dirs = []
    for o in all_objects:
        if isfile(join(path, o)):
            files.append(o)
        else:
            dirs.append(o)
    return files, dirs
