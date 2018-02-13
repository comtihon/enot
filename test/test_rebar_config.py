import unittest
from os.path import join

from mock import patch

from enot.packages.config.rebar import RebarConfig
from enot.packages.dep import Dep
from test.abs_test_class import TestClass


class RebarConfigTests(TestClass):
    def __init__(self, method_name):
        super().__init__('rebar_config_tests', method_name)

    def test_config_no_raw(self):
        with open(join(self.test_dir, 'rebar.config'), 'w') as config:
            config.write('''{deps, [
                              {dep1, ".*", {git, "git://github.com/comtihon/dep1"}},
                              {dep2, ".*", {git, "git://github.com/comtihon/dep2", {branch, "master"}}},
                              {dep3, ".*", {git, "git://github.com/comtihon/dep3", ""}},
                              {dep4, ".*", {git, "git://github.com/comtihon/dep4", {tag, "1.0.0"}}},
                              {dep5, ".*", {git, "git://github.com/comtihon/dep5", "commit_hash"}},
                              {dep6, ".*", {git, "git://github.com/comtihon/dep6", {ref, "commit_hash"}}}
                            ]}.''')
        conf = RebarConfig(self.test_dir)
        self.assertEqual(Dep('git://github.com/comtihon/dep1', 'master'), conf.deps['dep1'])
        self.assertEqual(Dep('git://github.com/comtihon/dep2', 'master'), conf.deps['dep2'])
        self.assertEqual(Dep('git://github.com/comtihon/dep3', 'HEAD'), conf.deps['dep3'])
        self.assertEqual(Dep('git://github.com/comtihon/dep4', 'master', '1.0.0'), conf.deps['dep4'])
        self.assertEqual(Dep('git://github.com/comtihon/dep5', 'commit_hash'), conf.deps['dep5'])
        self.assertEqual(Dep('git://github.com/comtihon/dep6', 'commit_hash'), conf.deps['dep6'])

    def test_config_no_vsn_no_raw(self):
        with open(join(self.test_dir, 'rebar.config'), 'w') as config:
            config.write('''{deps, [
                              {dep1, {git, "git://github.com/comtihon/dep1"}},
                              {dep2, {git, "git://github.com/comtihon/dep2", {branch, "master"}}},
                              {dep3, {git, "git://github.com/comtihon/dep3", ""}},
                              {dep4, {git, "git://github.com/comtihon/dep4", {tag, "1.0.0"}}},
                              {dep5, {git, "git://github.com/comtihon/dep5", "commit_hash"}},
                              {dep6, {git, "git://github.com/comtihon/dep6", {ref, "commit_hash"}}}
                            ]}.''')
        conf = RebarConfig(self.test_dir)
        self.assertEqual(Dep('git://github.com/comtihon/dep1', 'master'), conf.deps['dep1'])
        self.assertEqual(Dep('git://github.com/comtihon/dep2', 'master'), conf.deps['dep2'])
        self.assertEqual(Dep('git://github.com/comtihon/dep3', 'HEAD'), conf.deps['dep3'])
        self.assertEqual(Dep('git://github.com/comtihon/dep4', 'master', '1.0.0'), conf.deps['dep4'])
        self.assertEqual(Dep('git://github.com/comtihon/dep5', 'commit_hash'), conf.deps['dep5'])
        self.assertEqual(Dep('git://github.com/comtihon/dep6', 'commit_hash'), conf.deps['dep6'])

    @patch('enot.packages.config.config.request_hex_info')
    def test_hex_dep(self, mock_hex):
        mock_hex.return_value = {'url': 'https://hex.pm/api/packages/hex_dep',
                                 'updated_at': '2017-05-29T02:51:09.352157Z',
                                 'releases': [
                                     {'version': '1.0.0',
                                      'url': 'https://hex.pm/api/packages/hex_dep/releases/1.0.0',
                                      'updated_at': '2017-05-29T02:51:09.373260Z',
                                      'inserted_at': '2017-05-29T02:51:09.373254Z'}],
                                 'owners': [{'username': 'comtihon',
                                             'url': 'https://hex.pm/api/users/comtihon',
                                             'email': 'comtihon@test.com'}],
                                 'name': 'hex_dep',
                                 'meta': {'maintainers': ['Test'],
                                          'links': {'GitHub': 'https://github.com/comtihon/hex_dep'},
                                          'licenses': ['Apache 2'],
                                          'description': 'Just test'},
                                 'inserted_at': '2015-03-01T22:27:54.000000Z',
                                 'downloads': {'week': 0, 'day': 0, 'all': 0}}
        with open(join(self.test_dir, 'rebar.config'), 'w') as config:
            config.write('''{deps, [
                                {hex_dep, "1.0.0"}
                            ]}.''')
        conf = RebarConfig(self.test_dir)
        self.assertEqual(Dep('https://github.com/comtihon/hex_dep', None, tag='1.0.0'), conf.deps['hex_dep'])


if __name__ == '__main__':
    unittest.main()
