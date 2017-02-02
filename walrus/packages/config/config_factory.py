from os import listdir
from os.path import isfile, join

from walrus.packages.config import ErlangMkConfig, WalrusConfig, RebarConfig


def read_project(path):
    files = [f for f in listdir(path) if isfile(join(path, f))]
    if 'walrusfile.json' in files:
        return WalrusConfig(path)
    elif 'erlang.mk' in files:
        return ErlangMkConfig(path)
    elif 'rebar.config' in files:
        return RebarConfig(path)
