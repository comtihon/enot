from os.path import isfile

from os import listdir

from coon.packages.config.config import ConfigFile
from coon.packages.config.coon import CoonConfig
from coon.packages.config.erlang_mk import ErlangMkConfig
from coon.packages.config.rebar import RebarConfig


def read_project(path: str, vsn=None) -> ConfigFile:
    files = get_files(path)
    if 'coonfig.json' in files:
        return CoonConfig.from_path(path, vsn)
    elif 'erlang.mk' in files:
        return ErlangMkConfig(path, vsn)
    elif 'rebar.config' in files:
        return RebarConfig(path, vsn)
    raise ValueError("Unknown build system in project " + path)


def get_files(path: str) -> list:
    all_objects = listdir(path)
    return [d for d in all_objects if isfile(d)]
