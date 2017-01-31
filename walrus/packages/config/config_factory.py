from os import listdir
from os.path import isfile, join

from erl_terms.erl_terms_core import decode

from walrus.packages.config import ErlangMkConfig, config, WalrusConfig, RebarConfig


def read_project(path):
    name, vsn = project_preferenses(path)
    files = [f for f in listdir(path) if isfile(join(path, f))]
    project_config = None
    if 'walrusfile.json' in files:
        project_config = WalrusConfig(path, vsn)
    elif 'rebar.config' in files:
        project_config = RebarConfig(name, path, vsn)
    elif 'erlang.mk' in files:
        project_config = ErlangMkConfig(name, path, vsn)
    if project_config is not None:
        project_config.read_config()
    return project_config


def project_preferenses(path):
    appsrc = config.get_app_src(path)
    [decoded] = decode(appsrc)
    (_, name, opts) = decoded
    for (k, v) in opts:
        if k == 'vsn':
            return name, v
    print('No version specified!')
    return name, None
