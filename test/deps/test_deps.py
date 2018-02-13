import os
import unittest
from os.path import join

from mock import patch

import test
from enot.__main__ import create
from enot.pac_cache.local_cache import LocalCache
from enot.packages.package import Package
from enot.packages.package_builder import Builder
from test.abs_test_class import TestClass, set_deps


def mock_fetch_package(dep: Package):
    test_dir = test.get_test_dir('deps_tests')
    tmp_path = join(os.getcwd(), test_dir, 'tmp')
    if dep.name == 'update_dep' and dep.git_vsn == '1.2.0':
        set_deps(join(tmp_path, dep.name),
                 [
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '1.0.0'}
                 ])
    if dep.name == 'remove_dep' and dep.git_vsn == '1.2.0':
        set_deps(join(tmp_path, dep.name), [])
    dep.update_from_cache(join(tmp_path, dep.name))


class DepsTests(TestClass):
    def __init__(self, method_name):
        super().__init__('deps_tests', method_name)

    def setUp(self):
        super().setUp()
        create(self.test_dir, {'<name>': 'test_app'})

    # Test if deps and deps of deps are fetched properly
    @patch.object(LocalCache, 'fetch_package')
    @patch('enot.global_properties.ensure_conf_file')
    def test_deps_fetch(self, mock_conf, mock_fetch):
        mock_conf.return_value = self.conf_file
        mock_fetch.side_effect = mock_fetch_package
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep1',
                      'url': 'https://github.com/comtihon/dep1',
                      'tag': '1.0.0'},
                     {'name': 'dep2',
                      'url': 'https://github.com/comtihon/dep2',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep1'})
        create(self.tmp_dir, {'<name>': 'dep2'})
        dep_tmp_path = join(self.tmp_dir, 'dep2')
        set_deps(dep_tmp_path,
                 [
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep3'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(['dep1', 'dep2', 'dep3'].sort(), list(builder.packages.keys()).sort())
        self.assertEqual(mock_fetch.call_count, 3)

    # Test if some deps have same deps and they won't be fetched again
    @patch.object(LocalCache, 'fetch_package')
    @patch('enot.global_properties.ensure_conf_file')
    def test_deps_duplicates(self, mock_conf, mock_fetch):
        mock_conf.return_value = self.conf_file
        mock_fetch.side_effect = mock_fetch_package
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep1',
                      'url': 'https://github.com/comtihon/dep1',
                      'tag': '1.0.0'},
                     {'name': 'dep2',
                      'url': 'https://github.com/comtihon/dep2',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep1'})
        dep_tmp_path = join(self.tmp_dir, 'dep1')  # Here dep1 and dep2 have common dep3
        set_deps(dep_tmp_path,
                 [
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep2'})
        dep_tmp_path = join(self.tmp_dir, 'dep2')
        set_deps(dep_tmp_path,
                 [
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep3'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(['dep1', 'dep2', 'dep3'].sort(), list(builder.packages.keys()).sort())
        self.assertEqual(mock_fetch.call_count, 3)  # dep3 was fetched only once

    # Test if some low level deps require up level deps
    @patch.object(LocalCache, 'fetch_package')
    @patch('enot.global_properties.ensure_conf_file')
    def test_circular_deps(self, mock_conf, mock_fetch):
        mock_conf.return_value = self.conf_file
        mock_fetch.side_effect = mock_fetch_package
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep1',
                      'url': 'https://github.com/comtihon/dep1',
                      'tag': '1.0.0'},
                     {'name': 'dep2',
                      'url': 'https://github.com/comtihon/dep2',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep1'})
        create(self.tmp_dir, {'<name>': 'dep2'})
        dep_tmp_path = join(self.tmp_dir, 'dep2')
        set_deps(dep_tmp_path,
                 [
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep3'})
        dep_tmp_path = join(self.tmp_dir, 'dep3')
        set_deps(dep_tmp_path,  # dep3 require dep1, which was included in root.
                 [
                     {'name': 'dep1',
                      'url': 'https://github.com/comtihon/dep1',
                      'tag': '1.0.0'}
                 ])
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(['dep1', 'dep2', 'dep3'].sort(), list(builder.packages.keys()).sort())
        self.assertEqual(mock_fetch.call_count, 3)  # dep1 was fetched only once

    # Test if root has newer dep and it will be selected over old version (if they are not in conflict)
    @patch.object(LocalCache, 'fetch_package')
    @patch('enot.global_properties.ensure_conf_file')
    def test_dep_override_newer(self, mock_conf, mock_fetch):
        mock_conf.return_value = self.conf_file
        mock_fetch.side_effect = mock_fetch_package
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep2',
                      'url': 'https://github.com/comtihon/dep2',
                      'tag': '1.0.0'},
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '1.0.1'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep2'})
        dep_tmp_path = join(self.tmp_dir, 'dep2')
        set_deps(dep_tmp_path,
                 [
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep3'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(['dep2', 'dep3'].sort(), list(builder.packages.keys()).sort())
        self.assertEqual(mock_fetch.call_count, 2)
        self.assertEqual('1.0.0', builder.packages['dep2'].git_vsn)
        self.assertEqual('1.0.1', builder.packages['dep3'].git_vsn)

    # Test if some deps have conflicting versions
    @patch.object(LocalCache, 'fetch_package')
    @patch('enot.global_properties.ensure_conf_file')
    def test_deps_conflict(self, mock_conf, mock_fetch):
        mock_conf.return_value = self.conf_file
        mock_fetch.side_effect = mock_fetch_package
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep1',
                      'url': 'https://github.com/comtihon/dep1',
                      'tag': '1.0.0'},
                     {'name': 'dep2',
                      'url': 'https://github.com/comtihon/dep2',
                      'tag': '1.0.0'},
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '2.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep1'})
        create(self.tmp_dir, {'<name>': 'dep2'})
        dep_tmp_path = join(self.tmp_dir, 'dep2')
        set_deps(dep_tmp_path,
                 [
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep3'})
        builder = Builder.init_from_path(pack_path)
        try:  # builder population should fail, because of conflicting dependencies.
            builder.populate()
            populated = True
        except RuntimeError:
            populated = False
        self.assertEqual(False, populated)

    # Test if newer deps will be prefered over old one if difference is not major
    @patch.object(LocalCache, 'fetch_package')
    @patch('enot.global_properties.ensure_conf_file')
    def test_deps_prefer_newer(self, mock_conf, mock_fetch):
        mock_conf.return_value = self.conf_file
        mock_fetch.side_effect = mock_fetch_package
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep1',
                      'url': 'https://github.com/comtihon/dep1',
                      'tag': '1.0.0'},
                     {'name': 'dep2',
                      'url': 'https://github.com/comtihon/dep2',
                      'tag': '1.0.0'},
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep2',
                      'tag': '1.0.0'},
                     {'name': 'dep4',
                      'url': 'https://github.com/comtihon/dep4',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep1'})
        dep_tmp_path = join(self.tmp_dir, 'dep1')
        set_deps(dep_tmp_path,
                 [
                     {'name': 'dep4',
                      'url': 'https://github.com/comtihon/dep4',
                      'tag': '1.0.1'}  # has newer version then root
                 ])
        create(self.tmp_dir, {'<name>': 'dep2'})
        dep_tmp_path = join(self.tmp_dir, 'dep2')
        set_deps(dep_tmp_path,
                 [
                     {'name': 'dep3',
                      'url': 'https://github.com/comtihon/dep3',
                      'tag': '1.1.0'}  # has newer version then root
                 ])
        create(self.tmp_dir, {'<name>': 'dep3'})
        create(self.tmp_dir, {'<name>': 'dep4'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(['dep1', 'dep2', 'dep3', 'dep4'].sort(), list(builder.packages.keys()).sort())
        self.assertEqual(mock_fetch.call_count, 6)  # root's deps were fetched, than 2 new deps were fetched
        self.assertEqual('1.0.0', builder.packages['dep1'].git_vsn)
        self.assertEqual('1.0.0', builder.packages['dep2'].git_vsn)
        self.assertEqual('1.1.0', builder.packages['dep3'].git_vsn)
        self.assertEqual('1.0.1', builder.packages['dep4'].git_vsn)

    # Test if newer dep has new dep which should also be fetched
    @patch.object(LocalCache, 'fetch_package')
    @patch('enot.global_properties.ensure_conf_file')
    def test_deps_prefer_newer_new_dep(self, mock_conf, mock_fetch):
        mock_conf.return_value = self.conf_file
        mock_fetch.side_effect = mock_fetch_package
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'update_dep',
                      'url': 'https://github.com/comtihon/update_dep',
                      'tag': '1.0.0'},  # has no deps in 1.0.0
                     {'name': 'dep2',
                      'url': 'https://github.com/comtihon/dep2',
                      'tag': '1.0.0'}  # dep2 has update_dep 1.2.0 in deps
                 ])
        create(self.tmp_dir, {'<name>': 'dep2'})
        dep_tmp_path = join(self.tmp_dir, 'dep2')
        set_deps(dep_tmp_path,
                 [
                     {'name': 'update_dep',
                      'url': 'https://github.com/comtihon/update_dep',
                      'tag': '1.2.0'}  # has newer version then root
                 ])
        create(self.tmp_dir, {'<name>': 'update_dep'})  # dep3 will be added as a dep in 1.2.0 (see mock_fetch_package)
        create(self.tmp_dir, {'<name>': 'dep3'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(['update_dep', 'dep2', 'dep3'].sort(), list(builder.packages.keys()).sort())
        self.assertEqual('1.0.0', builder.packages['dep2'].git_vsn)
        self.assertEqual('1.2.0', builder.packages['update_dep'].git_vsn)
        self.assertEqual('1.0.0', builder.packages['dep3'].git_vsn)


if __name__ == '__main__':
    unittest.main()
