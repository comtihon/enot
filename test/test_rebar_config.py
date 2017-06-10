import unittest

from os.path import join

from coon.packages.dep import Dep

from coon.packages.config.rebar import RebarConfig

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


if __name__ == '__main__':
    unittest.main()
