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
from enot.tool.relxtool import RelxTool
from enot.utils.file_utils import ensure_dir
from test.abs_test_class import TestClass, set_deps, set_link_policy, ensure_tool


def mock_fetch_package(dep: Package):
    test_dir = test.get_test_dir('build_tests')
    tmp_path = join(os.getcwd(), test_dir, 'tmp')
    dep.update_from_cache(join(tmp_path, dep.name))


class BuildTests(TestClass):
    def __init__(self, method_name):
        super().__init__('build_tests', method_name)

    def setUp(self):
        super().setUp()
        create(self.test_dir, {'<name>': 'test_app'})

    # Dep should be linked to the project
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_link_dep(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep_with_no_deps',
                      'url': 'https://github.com/comtihon/dep_with_no_deps',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep_with_no_deps'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        dep_link_ebin = join(pack_path, 'deps', 'dep_with_no_deps', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Static.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep_with_no_deps', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))

    # Dep's dep should also be linked to the project
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_link_multiple_deps(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
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
        # Create dep B (in tmp, as if we download them from git)
        create(self.tmp_dir, {'<name>': 'b_with_no_deps'})
        # Create dep A2 (in tmp, as if we download them from git)
        create(self.tmp_dir, {'<name>': 'a2_with_no_deps'})
        # Compile test_project
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        for dep in ['a_with_dep_a2', 'b_with_no_deps', 'a2_with_no_deps']:
            print('Check ' + dep)
            dep_link_ebin = join(pack_path, 'deps', dep, 'ebin')
            self.assertEqual(True, os.path.islink(dep_link_ebin))
            erl = Static.get_erlang_version()
            real_dep = join(self.cache_dir, 'comtihon', dep, '1.0.0', erl, 'ebin')
            self.assertEqual(real_dep, os.readlink(dep_link_ebin))

    # Dep's dep should not be linked to the project, as it is prohibited by config
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_link_multiple_deps_off(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
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
        set_link_policy(pack_path, False)
        # Create dep A with dep A2 (in tmp, as if we download them from git)
        create(self.tmp_dir, {'<name>': 'a_with_dep_a2'})
        dep_a1_path = join(self.tmp_dir, 'a_with_dep_a2')
        set_deps(dep_a1_path, [{'name': 'a2_with_no_deps',
                                'url': 'https://github.com/comtihon/a2_with_no_deps',
                                'tag': '1.0.0'}])
        # Create dep B (in tmp, as if we download them from git)
        create(self.tmp_dir, {'<name>': 'b_with_no_deps'})
        # Create dep A2 (in tmp, as if we download them from git)
        create(self.tmp_dir, {'<name>': 'a2_with_no_deps'})
        # Compile test_project
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        for dep in ['a_with_dep_a2', 'b_with_no_deps']:
            print('Check ' + dep)
            dep_link_ebin = join(pack_path, 'deps', dep, 'ebin')
            self.assertEqual(True, os.path.islink(dep_link_ebin))
            erl = Static.get_erlang_version()
            real_dep = join(self.cache_dir, 'comtihon', dep, '1.0.0', erl, 'ebin')
            self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        dep_dep_link_ebin = join(pack_path, 'deps', 'a2_with_no_deps', 'ebin')
        self.assertEqual(False, os.path.islink(dep_dep_link_ebin))

    # Dep should be included to the release
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_release_dep(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        path = ensure_tool(RelxTool())
        builder.system_config.cache.local_cache.add_tool('relx', path)
        self.assertEqual(True, builder.release())
        lib_dir = join(pack_path, '_rel', 'test_app', 'lib')
        self.assertEqual(True, os.path.exists(join(lib_dir, 'dep-0.0.1')))

    # Dep's dep should be included to the release
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_release_multiple_deps(self, mock_conf, _):
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
        path = ensure_tool(RelxTool())
        builder.system_config.cache.local_cache.add_tool('relx', path)
        self.assertEqual(True, builder.release())
        lib_dir = join(pack_path, '_rel', 'test_app', 'lib')
        self.assertEqual(True, os.path.exists(join(lib_dir, 'dep-0.0.1')))
        self.assertEqual(True, os.path.exists(join(lib_dir, 'dep_with_dep-0.0.1')))

    # Non OTP deps should be included to the release
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_release_non_otp_dep(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])

        create(self.tmp_dir, {'<name>': 'dep'})
        dep_dir = join(self.tmp_dir, 'dep')
        ensure_dir(join(dep_dir, 'src'))
        with open(join(dep_dir, 'src', 'dep.erl'), 'w') as f:
            f.write('''
                    -module(dep).
                    -export([test/0]).
                    test() ->
                        ok.
                    ''')
        with open(join(dep_dir, 'src', 'dep.app.src'), 'w') as f:
            f.write('''
                    {application, dep, [
                      {description, ""},
                      {vsn, "0.0.1"},
                      {registered, []},
                      {applications, {{ app.std_apps + app.apps }}},
                      {modules, {{ modules }}}
                    ]}.
                    ''')
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        path = ensure_tool(RelxTool())
        builder.system_config.cache.local_cache.add_tool('relx', path)
        self.assertEqual(True, builder.release())
        lib_dir = join(pack_path, '_rel', 'test_app', 'lib')
        self.assertEqual(True, os.path.exists(join(lib_dir, 'dep-0.0.1')))
        # TODO add test for real non otp apps with no src/ebin dir and custom makefile.

    # If project has test_deps, they should not be fetched and built during normal build
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_project_with_test_dep_normal_build(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        set_deps(pack_path,
                 [
                     {'name': 'test_dep',
                      'url': 'https://github.com/comtihon/test_dep',
                      'tag': '1.0.0'}
                 ],
                 'test_deps')
        create(self.tmp_dir, {'<name>': 'dep'})
        builder = Builder.init_from_path(pack_path)
        builder.populate()
        self.assertEqual(True, builder.build())
        dep_link_ebin = join(pack_path, 'deps', 'dep', 'ebin')
        self.assertEqual(True, os.path.islink(dep_link_ebin))
        erl = Static.get_erlang_version()
        real_dep = join(self.cache_dir, 'comtihon', 'dep', '1.0.0', erl, 'ebin')
        self.assertEqual(real_dep, os.readlink(dep_link_ebin))
        dep_link_ebin = join(pack_path, 'deps', 'test_dep', 'ebin')
        self.assertEqual(False, os.path.islink(dep_link_ebin))

    # If project has test_deps, they should be fetched, built and link during test build
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_project_with_test_dep_test_build(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        set_deps(pack_path,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        set_deps(pack_path,
                 [
                     {'name': 'test_dep',
                      'url': 'https://github.com/comtihon/test_dep',
                      'tag': '1.0.0'}
                 ],
                 'test_deps')
        create(self.tmp_dir, {'<name>': 'dep'})
        create(self.tmp_dir, {'<name>': 'test_dep'})
        builder = Builder.init_from_path(pack_path)
        builder.populate(include_test_deps=True)
        self.assertEqual(True, builder.build())
        for dep in ['dep', 'test_dep']:
            print('Check ' + dep)
            dep_link_ebin = join(pack_path, 'deps', dep, 'ebin')
            self.assertEqual(True, os.path.islink(dep_link_ebin))
            erl = Static.get_erlang_version()
            real_dep = join(self.cache_dir, 'comtihon', dep, '1.0.0', erl, 'ebin')
            self.assertEqual(real_dep, os.readlink(dep_link_ebin))


if __name__ == '__main__':
    unittest.main()
