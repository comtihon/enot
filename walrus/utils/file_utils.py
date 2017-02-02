import os
import shutil
from os.path import join
from erl_terms.erl_terms_core import decode


def read_file(path: str) -> str:
    with open(path, 'r') as f:
        return f.read()


# TODO catch read errors
def read_file_lines(path: str) -> list:
    with open(path, 'r') as f:
        return f.readlines()


def write_file_lines(file_lines: list, path: str):
    with open(path, 'w') as f:
        f.writelines(file_lines)


# TODO catch write errors
def write_file(path: str, content: str) -> str:
    with open(path, 'w') as f:
        f.write(content)
    return path


def if_dir_exists(path: str, dir_to_check: str) -> str or None:
    full = join(path, dir_to_check)
    if not os.path.exists(full):
        return None
    else:
        return full


def link_if_needed(include_src, include_dst):
    if os.path.exists(include_src) and not os.path.exists(include_dst):
        os.symlink(include_src, include_dst)


def ensure_dir(path: str):
    if not os.path.exists(path):
        os.makedirs(path)


def remove_dir(path: str):
    if os.path.exists(path):
        shutil.rmtree(path)


# TODO may be searching directly by name is more efficient. But also can be less reliable.
def read_erlang_file(path: str, app: str) -> list or False:
    res = [f for f in os.listdir(path) if join(path, f).endswith(app)]
    if not res:
        return False
    elif len(res) == 1:
        [fileapp] = res
        file_str = read_file(join(path, fileapp))
        return decode(file_str)
    else:
        print('more than one app.src in application!')
        raise RuntimeError('More than one app.src in app')
