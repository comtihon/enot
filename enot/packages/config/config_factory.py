from os import listdir
from os.path import isfile, join

from enot.packages.config.config import ConfigFile
from enot.packages.config.enot import EnotConfig
from enot.packages.config.erlang_mk import ErlangMkConfig
from enot.packages.config.rebar import RebarConfig


def read_project(path: str, url=None) -> ConfigFile:
    files = get_files(path)
    if 'enot_config.json' in files:
        return EnotConfig.from_path(path, url=url)
    elif 'erlang.mk' in files:
        return ErlangMkConfig(path, url=url)
    elif 'rebar.config' in files:
        return RebarConfig(path, url=url)
    raise ValueError("Unknown build system in project " + path)


def get_files(path: str) -> list:
    all_objects = listdir(path)
    return [d for d in all_objects if isfile(join(path, d))]
