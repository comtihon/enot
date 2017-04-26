from os.path import join

import os

from coon.utils.file_utils import read_file


def parse_app_config(path: str, suffix: str) -> (str, str or None, list or None):
    file = find_app_file(path, suffix)
    content = read_file(join(path, file))
    name = find_app_name(content)
    vsn = find_app_vsn(content)
    apps = find_apps(content)
    return name, vsn, apps


def find_app_file(path, suffix):
    res = [f for f in os.listdir(path) if join(path, f).endswith(suffix)]
    if not res:
        raise FileNotFoundError('No ' + suffix + ' file in ' + path)
    if len(res) > 1:
        raise RuntimeError('More than one app.src file found: ' + str(res))
    return res[0]


def find_app_name(content: str) -> str:
    [_, rest] = str.split(content, '{application,')
    [name, _] = str.split(rest, ',', maxsplit=1)
    return name.strip()


def find_app_vsn(content: str) -> str or None:
    [_, rest] = str.split(content, '{vsn')
    [vsn, _] = str.split(rest, '}', maxsplit=1)
    if '{{' in vsn:  # if jinja template
        return None
    return vsn.strip(', "')


def find_apps(content: str) -> list or None:
    [_, rest] = str.split(content, '{applications,')
    [_, start] = str.split(rest, '[', maxsplit=1)
    [apps, _] = str.split(start, ']', maxsplit=1)
    if '{{' in apps:  # if jinja2 template
        return None
    return [dep.strip() for dep in apps.split(',')]
