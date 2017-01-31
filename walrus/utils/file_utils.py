import os
import shutil
from os.path import join


def read_file(path: str) -> str:
    with open(path, 'r') as f:
        return f.read()


# TODO catch read errors
def read_file_lines(path: str) -> list:
    with open(path, 'r') as f:
        return f.readlines()


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
