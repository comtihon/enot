import unittest
from os.path import join

import os
from mock import patch

import test
from coon.__main__ import create
from coon.pac_cache.cache import Cache
from coon.pac_cache.local_cache import LocalCache
from coon.packages.package import Package
from coon.packages.package_builder import Builder
from test.abs_test_class import TestClass, set_deps, set_git_url, set_git_tag


def mock_fetch_package(dep: Package):
    test_dir = test.get_test_dir('upgrade_tests')
    tmp_path = join(os.getcwd(), test_dir, 'tmp')
    dep.update_from_cache(join(tmp_path, dep.name))


class UpgradeTests(TestClass):
    def __init__(self, method_name):
        super().__init__('upgrade_tests', method_name)

    def setUp(self):
        super().setUp()
        create(self.test_dir, {'<name>': 'test_app'})

    # If some library's tag was changed - on upgrade it should be re-linked
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('coon.global_properties.ensure_conf_file')
    def test_change_tag(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_no_deps',
                      'url': 'https://github.com/comtihon/dep_with_no_deps',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep_with_no_deps'})
        dep_dir = join(self.tmp_dir, 'dep_with_no_deps')
        set_git_url(dep_dir, 'http://github/comtihon/dep_with_no_deps')
        set_git_tag(dep_dir, '1.0.0')  # This is not needed but pretend we really fetch it from git
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_no_deps', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Cache.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_no_deps', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        # Dep version was changed
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_no_deps',
                      'url': 'https://github.com/comtihon/dep_with_no_deps',
                      'tag': '1.0.1'}
                 ])
        set_git_tag(dep_dir, '1.0.1')
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_no_deps', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Cache.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_no_deps', '1.0.1', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))

    # If on update some library get a new dep - it should be also included.
    # Download if not present in system, added to cache and linked to project
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('coon.global_properties.ensure_conf_file')
    def test_change_tag_new_dep(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_no_deps',
                      'url': 'https://github.com/comtihon/dep_with_no_deps',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep_with_no_deps'})
        dep_dir = join(self.tmp_dir, 'dep_with_no_deps')
        set_git_url(dep_dir, 'http://github/comtihon/dep_with_no_deps')
        set_git_tag(dep_dir, '1.0.0')  # This is not needed but pretend we really fetch it from git
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_no_deps', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Cache.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_no_deps', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        # Dep version was changed
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_no_deps',
                      'url': 'https://github.com/comtihon/dep_with_no_deps',
                      'tag': '1.0.1'}
                 ])
        set_git_tag(dep_dir, '1.0.1')
        set_deps(dep_dir,
                 [
                     {'name': 'new_dep',
                      'url': 'https://github.com/comtihon/new_dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'new_dep'})
        new_dep_dir = join(self.tmp_dir, 'new_dep')
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_no_deps', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Cache.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_no_deps', '1.0.1', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        self.assertEqual(True, builder.system_config.cache.exists_local(Package.from_path(new_dep_dir)))
        new_dep_link_ebin = join(pack_path, 'deps', 'new_dep', 'ebin')
        self.assertEqual(True, os.path.islink(new_dep_link_ebin))
        new_dep = join(self.cache_dir, 'comtihon', 'new_dep', '1.0.0', erl, 'ebin')
        self.assertEqual(new_dep, os.readlink(new_dep_link_ebin))

    # If on update some library got rid of dep - it should be removed (link only).
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('coon.global_properties.ensure_conf_file')
    def test_change_tag_remove_dep(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_dep',
                      'url': 'https://github.com/comtihon/dep_with_dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep_with_dep'})
        dep_dir = join(self.tmp_dir, 'dep_with_dep')
        set_git_url(dep_dir, 'http://github/comtihon/dep_with_dep')
        set_git_tag(dep_dir, '1.0.0')
        set_deps(dep_dir,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        # Check all two deps linked to the project
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Cache.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_dep', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        real_dep = join(self.cache_dir, 'comtihon', 'dep', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        # Dep version was changed - dep_with_dep no longer uses dep
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_dep',
                      'url': 'https://github.com/comtihon/dep_with_dep',
                      'tag': '1.0.1'}
                 ])
        set_deps(dep_dir, [])
        set_git_tag(dep_dir, '1.0.1')
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        # Check dep is not linked to project anymore
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_dep', '1.0.1', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(False, os.path.islink(dep_link_ebin))

    # If on update some library got rid of dep, but it is in use - it should not be removed.
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('coon.global_properties.ensure_conf_file')
    def test_change_tag_remove_using_dep(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_dep',
                      'url': 'https://github.com/comtihon/dep_with_dep',
                      'tag': '1.0.0'},
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep_with_dep'})
        dep_dir = join(self.tmp_dir, 'dep_with_dep')
        set_git_url(dep_dir, 'http://github/comtihon/dep_with_dep')
        set_git_tag(dep_dir, '1.0.0')
        set_deps(dep_dir,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        # Check all two deps linked to the project
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Cache.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_dep', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        real_dep = join(self.cache_dir, 'comtihon', 'dep', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        # Dep version was changed - dep_with_dep no longer uses dep
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_dep',
                      'url': 'https://github.com/comtihon/dep_with_dep',
                      'tag': '1.0.1'},
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        set_deps(dep_dir, [])
        set_git_tag(dep_dir, '1.0.1')
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        # Check dep is linked to project, as it is used by project
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_dep', '1.0.1', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))

    # If on update one library remove dep, but another library added same dep - it should not be removed.
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('coon.global_properties.ensure_conf_file')
    def test_change_tag_remove_add_dep(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_dep',
                      'url': 'https://github.com/comtihon/dep_with_dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep_with_dep'})
        dep_dir = join(self.tmp_dir, 'dep_with_dep')
        set_git_url(dep_dir, 'http://github/comtihon/dep_with_dep')
        set_git_tag(dep_dir, '1.0.0')
        set_deps(dep_dir,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        # Check all two deps linked to the project
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Cache.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_dep', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        real_dep = join(self.cache_dir, 'comtihon', 'dep', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        # Dep version was changed - dep_with_dep no longer uses dep, but project start using it
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_dep',
                      'url': 'https://github.com/comtihon/dep_with_dep',
                      'tag': '1.0.1'},
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        set_deps(dep_dir, [])
        set_git_tag(dep_dir, '1.0.1')
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        # Check dep is linked to project, as it is used by project
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_dep', '1.0.1', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))

    # If erlang was updated - all deps should be recompiled and relinked.
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('coon.global_properties.ensure_conf_file')
    def test_update_erlang_relink_deps(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_no_deps',
                      'url': 'https://github.com/comtihon/dep_with_no_deps',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep_with_no_deps'})
        dep_dir = join(self.tmp_dir, 'dep_with_no_deps')
        set_git_url(dep_dir, 'http://github/comtihon/dep_with_no_deps')
        set_git_tag(dep_dir, '1.0.0')  # This is not needed but pretend we really fetch it from git
        with patch.object(Cache, 'get_erlang_version', return_value='18'):
            builder = Builder.init_from_path(pack_path)
            builder.populate()
            self.assertEqual(True, builder.build())
            dep_link_ebin = join(pack_path, 'deps', 'dep_with_no_deps', 'ebin')
            self.assertEqual(True, os.path.islink(dep_link_ebin))
            erl = Cache.get_erlang_version()
            real_dep = join(self.cache_dir, 'comtihon', 'dep_with_no_deps', '1.0.0', erl, 'ebin')
            self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        with patch.object(Cache, 'get_erlang_version', return_value='22'):
            builder = Builder.init_from_path(pack_path)
            builder.populate()
            self.assertEqual(True, builder.build())
            dep_link_ebin = join(pack_path, 'deps', 'dep_with_no_deps', 'ebin')
            self.assertEqual(True, os.path.islink(dep_link_ebin))
            erl2 = Cache.get_erlang_version()
            real_dep = join(self.cache_dir, 'comtihon', 'dep_with_no_deps', '1.0.0', erl2, 'ebin')
            self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        self.assertNotEqual(erl, erl2)

    # If branch was changed - should fetch, build and link new branch.
    def test_change_branch(self):
        True

    # If branch gets new commit - it should be updated and relinked
    def test_update_branch(self):
        True

    # If branch gets new commit, but lock points to old branch - no update should be performed.
    def test_update_branch_lock(self):
        True

    # If branch was updated N+1 times, only last N branches should be present in local cache
    def test_update_branch_last_n(self):
        True


if __name__ == '__main__':
    unittest.main()
