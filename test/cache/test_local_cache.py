import os
import unittest
from os.path import join

from mock import patch

import test
from enot.__main__ import create
from enot.pac_cache import Static
from enot.pac_cache.local_cache import LocalCache
from enot.packages.package import Package
from enot.packages.package_builder import Builder
from enot.utils.file_utils import remove_dir, copy_file
from test.abs_test_class import TestClass, set_deps, set_git_url, set_git_tag, modify_config


def mock_fetch_package(dep: Package):
    test_dir = test.get_test_dir('local_cache_tests')
    tmp_path = join(os.getcwd(), test_dir, 'tmp')
    dep.update_from_cache(join(tmp_path, dep.name))


class LocalCacheTests(TestClass):
    def __init__(self, method_name):
        super().__init__('local_cache_tests', method_name)

    def setUp(self):
        super().setUp()
        create(self.test_dir, {'<name>': 'test_app'})

    # Tests if package exists in local cache
    @patch('enot.global_properties.ensure_conf_file')
    def test_package_exists(self, mock_conf):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_git_url(pack_path, 'http://github/comtihon/test_app')
        set_git_tag(pack_path, '1.0.0')
        pack = Package.from_path(pack_path)
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(False, builder.system_config.cache.exists_local(pack))
        self.assertEqual(True, builder.build())
        builder.system_config.cache.add_package_local(pack)
        self.assertEqual(True, builder.system_config.cache.exists_local(pack))

    # Test if test_app.ep can be added to local cache
    @patch('enot.global_properties.ensure_conf_file')
    def test_add_from_package(self, mock_conf):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_git_url(pack_path, 'http://github/comtihon/test_app')
        set_git_tag(pack_path, '1.0.0')
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(True, builder.build())
        builder.package()
        new_package_path = join(self.test_dir, 'test_app.ep')
        # remove source project, test should work only with enot package
        copy_file(join(pack_path, 'test_app.ep'), new_package_path)
        remove_dir(pack_path)
        package = Package.from_package(new_package_path)
        self.assertEqual(False, builder.system_config.cache.exists_local(package))
        # local cache is used here to determine tmp dir
        local_cache = builder.system_config.cache.local_cache
        builder.system_config.cache.add_fetched(local_cache, package)
        self.assertEqual(True, builder.system_config.cache.exists_local(package))

    # Test if test_app can be added to local cache
    @patch('enot.global_properties.ensure_conf_file')
    def test_add_from_path(self, mock_conf):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_git_url(pack_path, 'http://github/comtihon/test_app')
        set_git_tag(pack_path, '1.0.0')
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(True, builder.build())
        self.assertEqual(False, builder.system_config.cache.exists_local(builder.project))
        builder.system_config.cache.add_package_local(builder.project)
        self.assertEqual(True, builder.system_config.cache.exists_local(builder.project))

    # Test if dep is fetched, compiled and linked to the project
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_link_dep(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep'})
        dep_tmp_path = join(self.tmp_dir, 'dep')
        set_git_url(dep_tmp_path, 'http://github/comtihon/dep')
        set_git_tag(dep_tmp_path, '1.0.0')
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        # dep was added to cache
        self.assertEqual(True, builder.system_config.cache.exists_local(Package.from_path(dep_tmp_path)))
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Static.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))

    # Test if dep exists in local cache and is linked to project
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_link_existing_dep(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep'})
        dep_path = join(self.tmp_dir, 'dep')
        set_git_url(dep_path, 'https://github/comtihon/dep')
        set_git_tag(dep_path, '1.0.0')
        dep_builder = Builder.init_from_path(dep_path)
        self.assertEqual(True, dep_builder.build())
        dep_builder.system_config.cache.add_package_local(dep_builder.project)
        self.assertEqual(True, dep_builder.system_config.cache.exists_local(dep_builder.project))
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Static.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))

    # Test if test_app has several deps, all will be fetched, compiled and added to local cache
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_add_with_deps(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        # Create test_app with deps: A and B
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'a_with_dep_a2',
                      'url': 'https://github.com/comtihon/a_with_dep_a2',
                      'tag': '1.0.0'},
                     {'name': 'b_with_no_deps',
                      'url': 'https://github.com/comtihon/b_with_no_deps',
                      'tag': '1.0.0'}
                 ])
        # Create dep A with dep A2 (in tmp, as if we download them from git)
        create(self.tmp_dir, {'<name>': 'a_with_dep_a2'})
        dep_a1_path = join(self.tmp_dir, 'a_with_dep_a2')
        set_deps(dep_a1_path, [{'name': 'a2_with_no_deps',
                                'url': 'https://github.com/comtihon/a2_with_no_deps',
                                'tag': '1.0.0'}])
        set_git_url(dep_a1_path, 'http://github/comtihon/a2_with_no_deps')
        set_git_tag(dep_a1_path, '1.0.0')
        # Create dep B (in tmp, as if we download them from git)
        create(self.tmp_dir, {'<name>': 'b_with_no_deps'})
        dep_b_path = join(self.tmp_dir, 'b_with_no_deps')
        set_git_url(dep_b_path, 'http://github/comtihon/b_with_no_deps')
        set_git_tag(dep_b_path, '1.0.0')
        # Create dep A2 (in tmp, as if we download them from git)
        create(self.tmp_dir, {'<name>': 'a2_with_no_deps'})
        dep_a2_path = join(self.tmp_dir, 'a2_with_no_deps')
        set_git_url(dep_a2_path, 'http://github/comtihon/a2_with_no_deps')
        set_git_tag(dep_a2_path, '1.0.0')
        # Compile test_project
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(False, builder.system_config.cache.exists_local(Package.from_path(dep_a1_path)))
        self.assertEqual(False, builder.system_config.cache.exists_local(Package.from_path(dep_b_path)))
        self.assertEqual(False, builder.system_config.cache.exists_local(Package.from_path(dep_a2_path)))
        builder.populate()
        self.assertEqual(True, builder.build())
        self.assertEqual(True, builder.system_config.cache.exists_local(Package.from_path(dep_a1_path)))
        self.assertEqual(True, builder.system_config.cache.exists_local(Package.from_path(dep_b_path)))
        self.assertEqual(True, builder.system_config.cache.exists_local(Package.from_path(dep_a2_path)))

    # Test if dep exists in local cache and is linked to project
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_link_with_deps(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_dep',
                      'url': 'https://github.com/comtihon/dep_with_dep',
                      'tag': '1.0.0'}
                 ])
        # Create, build and add dep.
        create(self.tmp_dir, {'<name>': 'dep'})
        dep_dep_path = join(self.tmp_dir, 'dep')
        set_git_url(dep_dep_path, 'https://github/comtihon/dep')
        set_git_tag(dep_dep_path, '1.0.0')
        dep_builder = Builder.init_from_path(dep_dep_path)
        # Build dep of dep and add to cache
        dep_builder.populate()
        self.assertEqual(True, dep_builder.build())
        dep_builder.system_config.cache.add_package_local(dep_builder.project)
        self.assertEqual(True, dep_builder.system_config.cache.exists_local(dep_builder.project))
        # Create, build and add dep with dep: dep
        create(self.tmp_dir, {'<name>': 'dep_with_dep'})
        dep_path = join(self.tmp_dir, 'dep_with_dep')
        set_git_url(dep_path, 'https://github/comtihon/dep_with_dep')
        set_git_tag(dep_path, '1.0.0')
        set_deps(dep_path,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        dep_builder = Builder.init_from_path(dep_path)
        dep_builder.populate()
        self.assertEqual(True, dep_builder.build())
        dep_builder.system_config.cache.add_package_local(dep_builder.project)
        self.assertEqual(True, dep_builder.system_config.cache.exists_local(dep_builder.project))
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        for dep in ['dep_with_dep', 'dep']:  # Dep and dep's dep are linked to the project
            print('Check ' + dep)
            dep_link_ebin = join(pack_path, 'deps', dep, 'ebin')
            self.assertEqual(True, os.path.islink(dep_link_ebin))
            erl = Static.get_erlang_version()
            real_dep = join(self.cache_dir, 'comtihon', dep, '1.0.0', erl, 'ebin')
            self.assertEqual(real_dep, os.readlink(dep_link_ebin))

    # Local cache contains multiple package's versions and they all can be listed. Same with erl versions.
    @patch('enot.global_properties.ensure_conf_file')
    def test_versions_api(self, mock_conf):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_git_url(pack_path, 'http://github/comtihon/test_app')
        modify_config(pack_path, {'tag': '1.0.0'})
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(True, builder.build())
        builder.system_config.cache.add_package_local(builder.project)
        modify_config(pack_path, {'tag': '1.1.0'})
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(True, builder.build())
        builder.system_config.cache.add_package_local(builder.project)
        local_cache = builder.system_config.cache.local_cache
        self.assertEqual(['1.0.0', '1.1.0'], local_cache.get_versions('comtihon/test_app'))
        self.assertEqual([Static.get_erlang_version()], local_cache.get_erl_versions('comtihon/test_app', '1.1.0'))


if __name__ == '__main__':
    unittest.main()
