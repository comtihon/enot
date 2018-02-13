import json
import os
import unittest
from os.path import join

from mock import patch

from enot.__main__ import create, upgrade
from enot.pac_cache import Static
from enot.pac_cache.local_cache import LocalCache
from enot.packages.package_builder import Builder
from test.abs_test_class import TestClass, set_deps, set_git_url, set_git_tag


def mock_fetch_commit_1(url: str, _rev: str, path: str):
    splitted = path.split('/')
    [name] = splitted[-1:]
    create_path = '/'.join(splitted[:-1])
    create(create_path, {'<name>': name})
    set_git_url(path, url)
    return 'some_hash'


def mock_fetch_commit_2(url: str, rev: str, path: str):
    if rev != 'some_hash':
        raise RuntimeError('Locked commit\'s hash expected')
    mock_fetch_commit_1(url, rev, path)
    return 'some_hash'


def mock_fetch_commit_3(url: str, rev: str, path: str):
    if rev != 'master':
        raise RuntimeError('Master branch expected')
    mock_fetch_commit_1(url, rev, path)
    return 'some_other_hash'


class UpgradeTests(TestClass):
    def __init__(self, method_name):
        super().__init__('upgrade_tests', method_name)

    def setUp(self):
        super().setUp()
        create(self.test_dir, {'<name>': 'test_app'})

    # If branch gets new commit, but lock points to old branch - no update should be performed.
    @patch.object(LocalCache, 'fetch', side_effect=mock_fetch_commit_1)
    @patch('enot.global_properties.ensure_conf_file')
    def test_update_branch_lock(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_git_url(pack_path, 'http://github/comtihon/test_app')
        set_git_tag(pack_path, '1.0.0')
        set_deps(pack_path,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'branch': 'master'}
                 ])
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Static.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep', 'master-some_hash', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        # lock was set
        locks_file = join(pack_path, 'enot_locks.json')
        self.assertEqual(True, os.path.isfile(locks_file))
        with open(locks_file, 'r') as file:
            locks = json.load(file)
        self.assertEqual('master-some_hash', locks['comtihon/dep'])
        # populate should not crash, as some_hash was locked
        with patch.object(LocalCache, 'fetch', side_effect=mock_fetch_commit_2):
            builder.populate()

    # Running upgrade should ignore locks
    @patch.object(LocalCache, 'fetch', side_effect=mock_fetch_commit_1)
    @patch('enot.global_properties.ensure_conf_file')
    def test_update_branch(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_git_url(pack_path, 'http://github/comtihon/test_app')
        set_git_tag(pack_path, '1.0.0')
        set_deps(pack_path,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'branch': 'master'}
                 ])
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Static.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep', 'master-some_hash', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        # lock was set
        locks_file = join(pack_path, 'enot_locks.json')
        self.assertEqual(True, os.path.isfile(locks_file))
        with open(locks_file, 'r') as file:
            locks = json.load(file)
        self.assertEqual('master-some_hash', locks['comtihon/dep'])
        # drop all locks and do upgrade
        with patch.object(LocalCache, 'fetch', side_effect=mock_fetch_commit_3):
            upgrade(pack_path, {})
        self.assertEqual(True, os.path.isfile(locks_file))
        with open(locks_file, 'r') as file:
            locks = json.load(file)
        self.assertEqual('master-some_other_hash', locks['comtihon/dep'])


if __name__ == '__main__':
    unittest.main()
