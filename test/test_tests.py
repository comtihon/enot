import unittest
from os.path import join

from enot.__main__ import create
from enot.compiler.enot import EnotCompiler
from enot.packages.package import Package
from enot.utils.file_utils import ensure_dir
from test.abs_test_class import TestClass, set_deps


# TODO find a way to parse output of eunit to understand how much tests really pass/fail
# TODO parse test/logs for ct results
class TestingTests(TestClass):
    def __init__(self, method_name):
        super().__init__('testing_tests', method_name)

    def setUp(self):
        super().setUp()
        create(self.test_dir, {'<name>': 'test_app'})

    # Test that unit files are collected properly
    def test_collecting_units(self):
        test_dir = join(self.test_dir, 'test_app', 'test')
        ensure_dir(test_dir)
        with open(join(test_dir, 'first.erl'), 'w') as test:
            test.write('''
            -module(first).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?_assert(true).''')
        with open(join(test_dir, 'second.erl'), 'w') as test:
            test.write('''
            -module(second).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?_assert(true).''')
        package = Package.from_path(join(self.test_dir, 'test_app'))
        compiler = EnotCompiler(package)
        files = compiler._EnotCompiler__get_all_files(compiler.test_path, 'erl')
        self.assertEqual({'first': test_dir,
                          'second': test_dir}, files)

    # Test that unit files are collected properly in case of recurse
    def test_collecting_units_recurse(self):
        test_dir = join(self.test_dir, 'test_app', 'test')
        ensure_dir(test_dir)
        with open(join(test_dir, 'level1.erl'), 'w') as test:
            test.write('''
            -module(level1).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?_assert(true).''')
        subdir = join(test_dir, 'sub')
        ensure_dir(subdir)
        with open(join(subdir, 'level2.erl'), 'w') as test:
            test.write('''
            -module(level2).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?_assert(true).''')
        package = Package.from_path(join(self.test_dir, 'test_app'))
        compiler = EnotCompiler(package)
        files = compiler._EnotCompiler__get_all_files(compiler.test_path, 'erl')
        self.assertEqual({'level1': test_dir,
                          'level2': join(test_dir, 'sub')}, files)

    # Test if unit test pass
    def test_unit_test_ok(self):
        test_dir = join(self.test_dir, 'test_app', 'test')
        ensure_dir(test_dir)
        with open(join(test_dir, 'simple.erl'), 'w') as test:
            test.write('''
            -module(simple).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?_assert(true).''')
        package = Package.from_path(join(self.test_dir, 'test_app'))
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.unit())

    # Test if unit test fail
    def test_unit_test_fail(self):
        test_dir = join(self.test_dir, 'test_app', 'test')
        ensure_dir(test_dir)
        with open(join(test_dir, 'simple.erl'), 'w') as test:
            test.write('''
            -module(simple).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?assertEqual(true, false).''')
        package = Package.from_path(join(self.test_dir, 'test_app'))
        compiler = EnotCompiler(package)
        self.assertEqual(False, compiler.unit())

    # Test if several tests can pass
    def test_unit_multiple_test_ok(self, ):
        test_dir = join(self.test_dir, 'test_app', 'test')
        ensure_dir(test_dir)
        with open(join(test_dir, 'simple.erl'), 'w') as test:
            test.write('''
            -module(simple).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?_assert(true).''')
        subdir = join(test_dir, 'sub')
        ensure_dir(subdir)
        with open(join(subdir, 'level2.erl'), 'w') as test:
            test.write('''
            -module(level2).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?_assert(true).''')
        package = Package.from_path(join(self.test_dir, 'test_app'))
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.unit())

    # Test if one test can fail
    def test_unit_multiple_test_fail(self):
        test_dir = join(self.test_dir, 'test_app', 'test')
        ensure_dir(test_dir)
        with open(join(test_dir, 'simple.erl'), 'w') as test:
            test.write('''
            -module(simple).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?_assert(true).''')
        subdir = join(test_dir, 'sub')
        ensure_dir(subdir)
        with open(join(subdir, 'level2.erl'), 'w') as test:
            test.write('''
            -module(level2).
            -include_lib("eunit/include/eunit.hrl").

           run_test() ->
               ?assertEqual(true, false).''')
        package = Package.from_path(join(self.test_dir, 'test_app'))
        compiler = EnotCompiler(package)
        self.assertEqual(False, compiler.unit())

    # Test if common test pass
    def test_common_test_ok(self):
        test_dir = join(self.test_dir, 'test_app', 'test')
        ensure_dir(test_dir)
        with open(join(test_dir, 'common_SUITE.erl'), 'w') as test:
            test.write('''
            -module(common_SUITE).
            -include_lib("common_test/include/ct.hrl").
            -export([all/0]).
            -export([test/1]).
             
            all() -> [test].
             
            test(_Config) ->
                1 = 1.''')
        package = Package.from_path(join(self.test_dir, 'test_app'))
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.common('test/logs'))

    # Test if common test fails
    def test_common_test_fail(self):
        test_dir = join(self.test_dir, 'test_app', 'test')
        ensure_dir(test_dir)
        with open(join(test_dir, 'common_SUITE.erl'), 'w') as test:
            test.write('''
            -module(common_SUITE).
            -include_lib("common_test/include/ct.hrl").
            -export([all/0]).
            -export([test/1]).
             
            all() -> [test].
             
            test(_Config) ->
                1 = 2.''')
        package = Package.from_path(join(self.test_dir, 'test_app'))
        compiler = EnotCompiler(package)
        self.assertEqual(False, compiler.common('test/logs'))

    # Test if common test uses deps code
    def test_common_test_with_deps(self):
        app_dir = join(self.test_dir, 'test_app')
        dep_dir = join(app_dir, 'deps')
        set_deps(app_dir,  # root project depends on dep1
                 [
                     {'name': 'dep1',
                      'url': 'https://github.com/comtihon/dep1',
                      'tag': '1.0.0'}
                 ])
        create(dep_dir, {'<name>': 'dep1'})  # dep1 has dep_api:test/0
        with open(join(dep_dir, 'dep1', 'src', 'dep_api.erl'), 'w') as test:
            test.write('''
            -module(dep_api).
            -export([test/0]).
                          
            test() ->
                true.''')
        app_test_dir = join(app_dir, 'test')
        ensure_dir(app_test_dir)
        # dep_api:test/0 is used in common test of root project
        with open(join(app_test_dir, 'common_SUITE.erl'), 'w') as test:
            test.write('''
            -module(common_SUITE).
            -include_lib("common_test/include/ct.hrl").
            -export([all/0]).
            -export([test/1]).
             
            all() -> [test].
             
            test(_Config) ->
                true == dep_api:test().''')
        # Compile dep. I only do it in test, as in real life deps will be compiled and linked before running ct.
        dep = Package.from_path(join(dep_dir, 'dep1'))
        self.assertEqual(True, EnotCompiler(dep).compile())
        package = Package.from_path(app_dir)
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.common('test/logs'))

    # Test if unit test uses deps code
    def test_unit_test_with_deps(self):
        app_dir = join(self.test_dir, 'test_app')
        dep_dir = join(app_dir, 'deps')
        set_deps(app_dir,  # root project depends on dep1
                 [
                     {'name': 'dep1',
                      'url': 'https://github.com/comtihon/dep1',
                      'tag': '1.0.0'}
                 ])
        create(dep_dir, {'<name>': 'dep1'})  # dep1 has dep_api:test/0
        with open(join(dep_dir, 'dep1', 'src', 'dep_api.erl'), 'w') as test:
            test.write('''
            -module(dep_api).
            -export([test/0]).
                          
            test() ->
                true.''')
        app_test_dir = join(app_dir, 'test')
        ensure_dir(app_test_dir)
        # dep_api:test/0 is used in unit test of root project
        with open(join(app_test_dir, 'common.erl'), 'w') as test:
            test.write('''
            -module(common).
            -include_lib("eunit/include/eunit.hrl").

            run_test() ->
               ?assertEqual(true, dep_api:test()).''')
        # Compile dep. I only do it in test, as in real life deps will be compiled and linked before running unit.
        dep = Package.from_path(join(dep_dir, 'dep1'))
        self.assertEqual(True, EnotCompiler(dep).compile())
        package = Package.from_path(app_dir)
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.unit())

if __name__ == '__main__':
    unittest.main()
