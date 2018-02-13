import json
import os
import unittest
from os import listdir
from os.path import join

from git import Repo

import test
from enot.tool.tool import AbstractTool
from enot.utils import logger
from enot.utils.file_utils import ensure_empty, remove_dir


class TestClass(unittest.TestCase):
    def __init__(self, test_name, method_name):
        super().__init__(method_name)
        self._test_name = test_name
        self._test_dir = test.get_test_dir(test_name)
        logger.configure('debug')

    @property
    def test_name(self):
        return self._test_name

    @property
    def test_dir(self):
        return join(os.getcwd(), self._test_dir)

    @property
    def cache_dir(self):
        return join(self.test_dir, 'cache')

    @property
    def tmp_dir(self):
        return join(self.test_dir, 'tmp')

    @property
    def conf_file(self):
        return join(self.test_dir, 'global_config.json')

    @property
    def compiler(self) -> str:
        return 'enot'

    @property
    def global_config(self):
        return {'temp_dir': self.tmp_dir,
                'compiler': self.compiler,
                'cache': [
                    {
                        'name': 'local_cache',
                        'type': 'local',
                        'url': 'file://' + self.cache_dir
                    }]}

    def setUp(self):
        ensure_empty(test.get_test_dir(self.test_name))
        conf = self.global_config
        with open(self.conf_file, 'w') as outfile:
            json.dump(conf, outfile, sort_keys=True, indent=4)

    def tearDown(self):
        remove_dir(test.get_test_dir(self.test_name))

    def clear_local_cache(self):
        ensure_empty(self.cache_dir)


def set_deps(path: str, deps: list, dep_type='deps'):
    fullpath = join(path, 'enot_config.json')
    with open(fullpath, 'r') as file:
        conf = json.load(file)
    conf[dep_type] = deps
    with open(fullpath, 'w') as file:
        json.dump(conf, file)


def set_prebuild(path: str, prebuild: list, disable_prebuild=False, override_conf=False):
    modify_config(path, {'disable_prebuild': disable_prebuild,
                         'prebuild': prebuild,
                         'override': override_conf})


def set_link_policy(path: str, policy: bool):
    modify_config(path, {'link_all': policy})


def modify_config(path: str, override: dict):
    fullpath = join(path, 'enot_config.json')
    with open(fullpath, 'r') as file:
        conf = json.load(file)
    conf.update(override)
    with open(fullpath, 'w') as file:
        json.dump(conf, file)


def set_git_url(path: str, url: str):
    repo = Repo.init(path)
    repo.index.add(listdir(path))
    repo.index.commit("First commit")
    repo.create_head('master')
    repo.create_remote('origin', url=url)


def set_git_tag(path: str, tag: str):
    repo = Repo(path)
    repo.create_tag(tag, message='new tag')


def switch_branch(path: str, branch: str):
    repo = Repo(path)
    repo.create_head(branch)
    repo.git.checkout(branch)


# each tool should be downloaded once per all tests (to speed them up)
def ensure_tool(tool: AbstractTool):
    tool_path = join(test.TEST_DIR, tool.name)
    if os.path.isfile(tool_path):
        return tool_path
    return tool.ensure(test.TEST_DIR)
