import os
from os.path import join

from enot.utils.file_utils import read_file


# read application config file. Return application name, version, applications and if it contains jinja2 templates
def parse_app_config(path: str, suffix='.app.src') -> (str, str or None, list or None, bool):
    file = find_app_file(path, suffix)
    content = read_file(join(path, file))
    return parse_app_config_content(content)


def parse_app_config_content(content: str) -> (str, str or None, list or None, bool):
    name = find_app_name(content)
    vsn = find_app_vsn(content)
    apps = find_apps(content)
    return name, vsn, apps, '{{' in content


def contains_app_file(path: str, suffix='.app.src') -> bool:
    if not os.path.exists(path):
        return False
    res = [f for f in os.listdir(path) if join(path, f).endswith(suffix)]
    return res != []


# Will work only for comma last keys, without spaces between key and comma
def get_value(key: str, pos: int, content: str, maxsplit=-1) -> str:
    [_, rest] = str.split(content, '{' + key + ',')
    tokens = str.split(rest, ',', maxsplit=maxsplit)
    token = tokens[pos]
    if '}' in token:
        token = token.replace('}', '')
    return token.strip()


# Will work only for comma last keys, without spaces between key and comma
def get_values(key: str, content: str) -> list:
    [_, rest] = str.split(content, '{' + key + ',')
    [_, start] = str.split(rest, '[', maxsplit=1)
    [tokens, _] = str.split(start, ']', maxsplit=1)
    return [dep.strip() for dep in tokens.split(',')]


def parse_platform_define(value):
    if len(value) == 3:
        (_, case, define) = value
        return case, define
    elif len(value) == 4:
        (_, case, define, _) = value
        return case, define
    else:
        raise RuntimeError('Unknown configuration format: ' + value)


def find_app_file(path, suffix):
    if not os.path.exists(path):
        raise FileNotFoundError('No ' + suffix + ' file in ' + path)
    res = [f for f in os.listdir(path) if join(path, f).endswith(suffix)]
    if not res:
        raise FileNotFoundError('No ' + suffix + ' file in ' + path)
    if len(res) > 1:
        raise RuntimeError('More than one app.src file found: ' + str(res))
    return res[0]


def find_app_name(content: str) -> str:
    return get_value('application', 0, content, 1).replace("'", '')


def find_app_vsn(content: str) -> str or None:
    [_, rest] = str.split(content, '{vsn')
    [vsn, _] = str.split(rest, '}', maxsplit=1)
    if '{{' in vsn:  # if jinja template
        return None
    return vsn.strip(', "')


def find_apps(content: str) -> list or None:
    [_, rest] = str.split(content, '{applications,')
    if rest.strip().startswith('{{'):  # if jinja2 template (whole deps)
        return None
    [_, start] = str.split(rest, '[', maxsplit=1)
    [apps, _] = str.split(start, ']', maxsplit=1)
    if '{{' in apps:  # if jinja2 template (
        return None
    return [dep.strip('\' \r\n') for dep in apps.split(',') if dep is not '']
