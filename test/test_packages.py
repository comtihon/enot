import unittest
from os.path import join

from mock import patch

from enot.__main__ import create
from enot.packages.package import Package
from enot.packages.package_builder import Builder
from enot.packages.dep import Dep
from test.abs_test_class import TestClass, set_git_url, set_git_tag


class PackageTests(TestClass):
    def __init__(self, method_name):
        super().__init__('package_tests', method_name)

    # Package can be created from path. Usually by opening package source paths when working with project.
    def test_init_from_path(self):
        create(self.test_dir, {'<name>': 'test_app'})
        set_git_url(join(self.test_dir, 'test_app'), 'http://github/my_namespace/my_project')
        set_git_tag(join(self.test_dir, 'test_app'), '1.0.0')
        pack = Package.from_path(join(self.test_dir, 'test_app'))
        self.assertEqual('test_app', pack.name)
        self.assertEqual('0.0.1', pack.vsn)
        self.assertEqual('http://github/my_namespace/my_project', pack.url)
        self.assertEqual('1.0.0', pack.git_tag)
        self.assertEqual('master', pack.git_branch)
        self.assertEqual([], pack.deps)

    # Package can be created from dep. Usually when populating main project deps.
    def test_init_from_dep(self):
        pack = Package.from_dep('test_app', Dep('http://github/my_namespace/test_app', 'master', tag='1.0.0'))
        self.assertEqual([], pack.deps)
        self.assertEqual('test_app', pack.name)
        self.assertEqual('1.0.0', pack.git_tag)

    # Main project's dep can be fetched and filled.
    def test_update_from_cache(self):
        pack = Package.from_dep('my_dep', Dep('http://github/my_namespace/test_app', 'master', tag='1.0.0'))
        create(self.test_dir, {'<name>': 'my_dep'})
        pack.update_from_cache(join(self.test_dir, 'my_dep'))
        self.assertEqual('my_dep', pack.name)
        self.assertEqual('0.0.1', pack.vsn)
        self.assertEqual('1.0.0', pack.git_tag)
        self.assertEqual('http://github/my_namespace/test_app', pack.url)
        self.assertEqual([], pack.deps)

    # Package can be created from enot package tar file. Is usually called on manually load package to remote repo
    @patch('enot.global_properties.ensure_conf_file')
    def test_init_from_package(self, mock_conf):
        mock_conf.return_value = self.conf_file
        create(self.test_dir, {'<name>': 'my_dep'})
        pack_path = join(self.test_dir, 'my_dep')
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(True, builder.build())
        builder.package()
        pack = Package.from_package(join(pack_path, 'my_dep.ep'))
        self.assertEqual('my_dep', pack.name)
        self.assertEqual('0.0.1', pack.vsn)
        self.assertEqual([], pack.deps)

    # Package can be updated from enot package tar file, which was downloaded from dep.
    @patch('enot.global_properties.ensure_conf_file')
    def test_update_from_package(self, mock_conf):
        mock_conf.return_value = self.conf_file
        create(self.test_dir, {'<name>': 'my_dep'})
        pack_path = join(self.test_dir, 'my_dep')
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(True, builder.build())
        builder.package()
        pack = Package.from_dep('my_dep', Dep('http://github/my_namespace/test_app', 'master', tag='1.0.0'))
        pack.update_from_package(join(pack_path, 'my_dep.ep'))
        self.assertEqual('my_dep', pack.name)
        self.assertEqual('0.0.1', pack.vsn)
        self.assertEqual('1.0.0', pack.git_tag)
        self.assertEqual('master', pack.git_branch)
        self.assertEqual('http://github/my_namespace/test_app', pack.url)
        self.assertEqual([], pack.deps)


if __name__ == '__main__':
    unittest.main()
