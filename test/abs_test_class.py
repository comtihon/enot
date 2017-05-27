import json
import test
import unittest
from os import listdir
from os.path import join

import os
from git import Repo

from coon.utils.file_utils import ensure_empty, remove_dir


class TestClass(unittest.TestCase):
    def __init__(self, test_name, method_name):
        super().__init__(method_name)
        self._test_name = test_name
        self._test_dir = test.get_test_dir(test_name)

    @property
    def test_name(self):
        return self._test_name

    @property
    def test_dir(self):
        return self._test_dir

    @property
    def cache_dir(self):
        return join(os.getcwd(), self.test_dir, 'cache')

    @property
    def tmp_dir(self):
        return join(os.getcwd(), self.test_dir, 'tmp')

    @property
    def conf_file(self):
        return join(os.getcwd(), self.test_dir, 'global_config.json')

    @property
    def compiler(self) -> str:
        return 'coon'

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


def set_deps(path: str, deps: list):
    fullpath = join(path, 'coonfig.json')
    with open(fullpath, 'r') as file:
        conf = json.load(file)
    conf['deps'] = deps
    with open(fullpath, 'w') as file:
        json.dump(conf, file)


def set_git_url(path: str, url: str):
    repo = Repo.init(path)
    repo.index.add(listdir(path))
    repo.index.commit("First commit")
    repo.create_head('master')
    repo.create_remote('origin', url=url + '.git')


def set_git_tag(path: str, tag: str):
    repo = Repo(path)
    repo.create_tag(tag, message='new tag')
