import os
import subprocess
import unittest
from os import listdir
from os.path import join

from mock import patch

import test
from enot.__main__ import create
from enot.compiler.enot import EnotCompiler
from enot.pac_cache.local_cache import LocalCache
from enot.packages.config.enot import EnotConfig
from enot.packages.package import Package
from enot.packages.package_builder import Builder
from enot.utils.erl_file_utils import parse_app_config
from enot.utils.file_utils import ensure_dir
from test.abs_test_class import TestClass, set_prebuild, set_git_url, set_git_tag, set_deps


def mock_fetch_package(dep: Package):
    test_dir = test.get_test_dir('compile_tests')
    tmp_path = join(os.getcwd(), test_dir, 'tmp')
    dep.update_from_cache(join(tmp_path, dep.name))


class CompileTests(TestClass):
    def __init__(self, method_name):
        super().__init__('compile_tests', method_name)

    @property
    def src_dir(self):
        return join(self.test_dir, 'src')

    @property
    def ebin_dir(self):
        return join(self.test_dir, 'ebin')

    # Proper erlang file is compiled
    @patch.object(EnotCompiler, '_EnotCompiler__write_app_file')
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
        config = EnotConfig({'name': 'test'})
        package = Package(self.test_dir, config, None)
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(join(self.ebin_dir, 'proper.beam')))

    # Erlang file with syntax error is not compiled
    @patch.object(EnotCompiler, '_EnotCompiler__write_app_file')
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
        config = EnotConfig({'name': 'test'})
        package = Package(self.test_dir, config, None)
        compiler = EnotCompiler(package)
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
        with open(join(self.test_dir, 'enot_config.json'), 'w') as w:
            w.write('''{
            \"name\":\"proper\",
            \"app_vsn\":\"1.0.0\",
            \"deps\": [{\"name\": \"test_dep\",
                        \"url\": \"http://github/comtihon/test_dep\",
                        \"tag\": \"test_vsn\"}]
            }''')
        package = Package.from_path(self.test_dir)
        compiler = EnotCompiler(package)
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

    # if parse transform belongs to a project it will be built before module using it.
    @patch('enot.global_properties.ensure_conf_file')
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
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(join(project_dir, 'ebin')))

    # if config has some prebuild steps - they should be tun
    @patch('enot.global_properties.ensure_conf_file')
    def test_prebuild(self, mock_conf):
        mock_conf.return_value = self.conf_file
        create(self.tmp_dir, {'<name>': 'project'})
        project_dir = join(self.tmp_dir, 'project')
        test_file_path = join(project_dir, 'test_file')
        set_prebuild(project_dir, [{'shell': 'echo "test" > ' + test_file_path}])
        self.assertEqual(False, os.path.exists(test_file_path))
        package = Package.from_path(project_dir)
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(test_file_path))
        with open(test_file_path, 'r') as file:
            self.assertEqual('test\n', file.read())

    # if prebuild step is disabled - no prebuild will run
    @patch('enot.global_properties.ensure_conf_file')
    def test_disable_prebuild(self, mock_conf):
        mock_conf.return_value = self.conf_file
        create(self.tmp_dir, {'<name>': 'project'})
        project_dir = join(self.tmp_dir, 'project')
        test_file_path = join(project_dir, 'test_file')
        set_prebuild(project_dir,
                     [{'shell': 'echo "test" > ' + test_file_path}],
                     disable_prebuild=True)
        self.assertEqual(False, os.path.exists(test_file_path))
        package = Package.from_path(project_dir)
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(False, os.path.exists(test_file_path))

    # if root prebuild is disabled and override conf is true - no prebuild will run
    @patch.object(LocalCache, 'fetch_package', side_effect=mock_fetch_package)
    @patch('enot.global_properties.ensure_conf_file')
    def test_override_disable_prebuild(self, mock_conf, _):
        mock_conf.return_value = self.conf_file
        create(self.tmp_dir, {'<name>': 'project'})
        project_dir = join(self.tmp_dir, 'project')
        set_prebuild(project_dir, [], disable_prebuild=True, override_conf=True)
        # root project has dep, which has some shell prebuild step
        set_deps(project_dir,
                 [
                     {'name': 'dep',
                      'url': 'https://github.com/comtihon/dep',
                      'tag': '1.0.0'}
                 ])
        create(self.tmp_dir, {'<name>': 'dep'})
        dep_path = join(self.tmp_dir, 'dep')
        set_git_url(dep_path, 'https://github/comtihon/dep')
        set_git_tag(dep_path, '1.0.0')
        test_file_path = join(project_dir, 'test_file')
        set_prebuild(dep_path, [{'shell': 'echo "test" > ' + test_file_path}])
        builder = Builder.init_from_path(project_dir)
        builder.populate()
        self.assertEqual(True, builder.build())
        self.assertEqual(False, os.path.exists(test_file_path))  # no dep's prebuild step was executed

    # File is compiled with defined var
    @patch.object(EnotCompiler, '_EnotCompiler__write_app_file')
    def test_defines_setting(self, mock_compiler):
        mock_compiler.return_value = True
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'proper.erl'), 'w') as w:
            w.write('''
            -module(proper).
            -export([test/0]).
            test() -> io:format("~p~n", [?TEST_DEFINE]).
            ''')
        config = EnotConfig({'name': 'test'})
        package = Package(self.test_dir, config, None)
        compiler = EnotCompiler(package, 'TEST_DEFINE=test')
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(join(self.ebin_dir, 'proper.beam')))
        p = subprocess.Popen(['erl', '-pa', 'ebin', '-run', 'proper', 'test', '-run', 'init', 'stop', '-noshell'],
                             stdout=subprocess.PIPE,
                             cwd=self.ebin_dir)
        self.assertEqual(0, p.wait(5000))
        self.assertEqual('test\n', p.stdout.read().decode('utf8'))


if __name__ == '__main__':
    unittest.main()
