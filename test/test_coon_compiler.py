import unittest
from os.path import join

import os
from mock import patch
from os import listdir

import test
from coon.compiler.coon import CoonCompiler
from coon.packages.config.coon import CoonConfig
from coon.packages.package import Package
from coon.utils.erl_file_utils import parse_app_config
from coon.utils.file_utils import ensure_empty, ensure_dir
from test.abs_test_class import TestClass


class CompileTests(TestClass):
    def __init__(self, method_name):
        super().__init__('create_tests', method_name)

    @property
    def src_dir(self):
        return join(self.test_dir, 'src')

    @property
    def ebin_dir(self):
        return join(self.test_dir, 'ebin')

    def setUp(self):
        ensure_empty(test.get_test_dir(self.test_name))

    # Proper erlang file is compiled
    @patch.object(CoonCompiler, '_CoonCompiler__write_app_file')
    @patch.object(CoonConfig, 'read_config')
    def test_proper_compilation(self, mock_config, mock_compiler):
        mock_config.return_value = {}
        mock_compiler.return_value = True
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'proper.erl'), 'w') as w:
            w.write('''
            -module(proper).
            -export([test/0]).
            test() -> do_smth(1).
            do_smth(A) -> A + 1.
            ''')
        config = CoonConfig(self.test_dir)
        config.init_from_dict({'name': 'test'})
        package = Package(config=config)
        compiler = CoonCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(join(self.ebin_dir, 'proper.beam')))

    # Erlang file with syntax error is not compiled
    @patch.object(CoonCompiler, '_CoonCompiler__write_app_file')
    @patch.object(CoonConfig, 'read_config')
    def test_error_compilation(self, mock_config, mock_compiler):
        mock_config.return_value = {}
        mock_compiler.return_value = True
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'improper.erl'), 'w') as w:
            w.write('''
            -module(proper).
            -export([test/0]).
            test() -> syntax error here.
            do_smth(A) -> A + 1.
            ''')
        config = CoonConfig(self.test_dir)
        config.init_from_dict({'name': 'test'})
        package = Package(config=config)
        compiler = CoonCompiler(package)
        self.assertEqual(False, compiler.compile())
        self.assertEqual(False, os.path.exists(join(self.ebin_dir, 'improper.beam')))

    # application file is created from app.src file. Templates are filled.
    def test_write_app_file_from_src(self):
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'proper.erl'), 'w') as w:
            w.write('''
            -module(proper).
            -export([test/0]).
            test() -> do_smth(1).
            do_smth(A) -> A + 1.
            ''')
        with open(join(self.src_dir, 'proper.app.src'), 'w') as w:
            w.write('''
            {application, proper,
              [
                {description, ""},
                {vsn, "{{ app.vsn }}"},
                {registered, []},
                {modules, {{ modules }}},
                {applications, {{ app.std_apps + app.apps }}},
                {mod, {proper_app, []}},
                {env, []}
              ]}.
            ''')
        with open(join(self.test_dir, 'coonfig.json'), 'w') as w:
            w.write('''{
            \"name\":\"proper\",
            \"version\":\"1.0.0\",
            \"deps\": [{\"name\": \"test_dep\",
                        \"url\": \"test_url\",
                        \"vsn\": \"test_vsn\"}]
            }''')
        package = Package.from_path(self.test_dir)
        compiler = CoonCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(self.ebin_dir))
        ls = listdir(self.ebin_dir)
        self.assertEqual(True, 'proper.beam' in ls)
        self.assertEqual(True, 'proper.app' in ls)
        self.assertEqual(2, len(ls))
        (name, vsn, deps) = parse_app_config(self.ebin_dir, '.app')
        self.assertEqual('proper', name)
        self.assertEqual('1.0.0', vsn)
        self.assertEqual(deps, ['kernel', 'stdlib', 'test_dep'])


if __name__ == '__main__':
    unittest.main()
