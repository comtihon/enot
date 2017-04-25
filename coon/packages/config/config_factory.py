from os import listdir
from os.path import isfile, join

from coon.packages.config import ErlangMkConfig, CoonConfig, RebarConfig, ConfigFile


def read_project(path):
    files, dirs = split_files_and_dirs(path)
    if 'coonfig.json' in files:
        return CoonConfig(path, 'c_src' in dirs)
    elif 'erlang.mk' in files:
        return ErlangMkConfig(path, 'c_src' in dirs)
    elif 'rebar.config' in files:
        return RebarConfig(path, 'c_src' in dirs)
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
