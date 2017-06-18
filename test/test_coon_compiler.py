import unittest
from os.path import join

import os
from mock import patch
from os import listdir

import test
from coon.__main__ import create
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
    def test_proper_compilation(self, mock_compiler):
        mock_compiler.return_value = True
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'proper.erl'), 'w') as w:
            w.write('''
            -module(proper).
            -export([test/0]).
            test() -> do_smth(1).
            do_smth(A) -> A + 1.
            ''')
        config = CoonConfig({'name': 'test'})
        package = Package(self.test_dir, config, None)
        compiler = CoonCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(join(self.ebin_dir, 'proper.beam')))

    # Erlang file with syntax error is not compiled
    @patch.object(CoonCompiler, '_CoonCompiler__write_app_file')
    def test_error_compilation(self, mock_compiler):
        mock_compiler.return_value = True
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'improper.erl'), 'w') as w:
            w.write('''
            -module(proper).
            -export([test/0]).
            test() -> syntax error here.
            do_smth(A) -> A + 1.
            ''')
        config = CoonConfig({'name': 'test'})
        package = Package(self.test_dir, config, None)
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
            \"app_vsn\":\"1.0.0\",
            \"deps\": [{\"name\": \"test_dep\",
                        \"url\": \"http://github/comtihon/test_dep\",
                        \"tag\": \"test_vsn\"}]
            }''')
        package = Package.from_path(self.test_dir)
        compiler = CoonCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(self.ebin_dir))
        ls = listdir(self.ebin_dir)
        self.assertEqual(True, 'proper.beam' in ls)
        self.assertEqual(True, 'proper.app' in ls)
        self.assertEqual(2, len(ls))
        (name, vsn, deps, _) = parse_app_config(self.ebin_dir, '.app')
        self.assertEqual('proper', name)
        self.assertEqual('1.0.0', vsn)
        self.assertEqual(deps, ['kernel', 'stdlib', 'test_dep'])

    @patch('coon.global_properties.ensure_conf_file')
    def test_build_parse_transform_first(self, mock_conf):
        mock_conf.return_value = self.conf_file
        create(self.tmp_dir, {'<name>': 'project'})
        project_dir = join(self.tmp_dir, 'project')
        project_src = join(project_dir, 'src')
        with open(join(project_src, 'p_trans.erl'), 'w') as w:
            w.write('''
            -module(p_trans).
            -export([parse_transform/2]).
            
            -record(support, {add_fun = true, 
                              export = true}).
            
            parse_transform(AST, _Options) ->
              do_parse([], AST, #support{}).
              
            do_parse(AST, [], _) -> lists:reverse(AST);
            do_parse(AST, [F = {function, N, _, _, _} | Others], Support = #support{add_fun = true}) ->
              M = N - 1,
              AddedFun =
                {function, M, sum, 2,
                  [{clause, M,
                    [{var, M, 'A'}, {var, M, 'B'}],
                    [],
                    [{op, M, '+', {var, M, 'A'}, {var, M, 'B'}}]}]},
              TurnedOff = Support#support{add_fun = false},
              do_parse([F | [AddedFun | AST]], Others, TurnedOff);
            do_parse(AST, [E = {attribute, N, export, _} | Others], Support = #support{export = true}) ->
              Export = [E | AST],
              Exported = {attribute, N + 1, export, [{sum, 2}]},
              TurnedOff = Support#support{export = false},
              do_parse([Exported | Export], Others, TurnedOff);
            do_parse(AST, [H | Others], Support) ->
              do_parse([H | AST], Others, Support).
            ''')
        with open(join(project_src, 'a_module.erl'), 'w') as w:
            w.write('''
            -module(a_module).
            
            -compile([{parse_transform, p_trans}]).
            
            -export([hello/0]).
            
            hello() -> hello.
            ''')
        package = Package.from_path(project_dir)
        compiler = CoonCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(join(project_dir, 'ebin')))

if __name__ == '__main__':
    unittest.main()
