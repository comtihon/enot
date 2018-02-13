import subprocess
import unittest
from os.path import join
from subprocess import PIPE

import os

from enot.__main__ import create
from enot.compiler.c_compiler import CCompiler
from enot.compiler.enot import EnotCompiler
from enot.packages.config.enot import EnotConfig
from enot.packages.package import Package
from enot.utils.file_utils import ensure_dir
from test.abs_test_class import TestClass


class CCompileTests(TestClass):
    def __init__(self, method_name):
        super().__init__('c_compile_tests', method_name)

    @property
    def src_dir(self):
        return join(self.test_dir, 'c_src')

    @property
    def output_dir(self):
        return join(self.test_dir, 'priv')

    def test_proper_compilation(self):
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'proper.c'), 'w') as w:
            w.write('''
            #include "erl_nif.h"

            static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
            {
                return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
            }
            
            static ErlNifFunc nif_funcs[] =
            {
                {"hello", 0, hello}
            };
            
            ERL_NIF_INIT(niftest,nif_funcs,NULL,NULL,NULL,NULL)
            ''')
        config = EnotConfig({'name': 'proper'})
        package = Package(self.test_dir, config, None)
        compiler = CCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(join(self.output_dir, self.test_name + '.so')))

    def test_error_compilation(self):
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'proper.c'), 'w') as w:
            w.write('''
            #include "erl_nif.h"

            static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
            {
                return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1)  // error here, forget terminator
            }
            
            static ErlNifFunc nif_funcs[] =
            {
                {"hello", 0, hello}
            };
            
            ERL_NIF_INIT(niftest,nif_funcs,NULL,NULL,NULL,NULL)
            ''')
        config = EnotConfig({'name': 'proper'})
        package = Package(self.test_dir, config, None)
        compiler = CCompiler(package)
        self.assertEqual(False, compiler.compile())
        self.assertEqual(False, os.path.exists(join(self.output_dir, self.test_name + '.so')))

    def test_proper_project_compilation(self):
        create(self.test_dir, {'<name>': 'proper'})
        project_dir = join(self.test_dir, 'proper')
        ensure_dir(join(project_dir, 'c_src'))
        with open(join(project_dir, 'c_src/proper.c'), 'w') as w:
            w.write('''
            #include "erl_nif.h"

            static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
            {
                return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
            }
            
            static ErlNifFunc nif_funcs[] =
            {
                {"hello", 0, hello}
            };
            
            ERL_NIF_INIT(proper,nif_funcs,NULL,NULL,NULL,NULL)
            ''')
        with open(join(project_dir, 'src/proper.erl'), 'w') as w:
            w.write('''
            -module(proper).

            -export([init/0, hello/0]).
            
            init() ->
                  erlang:load_nif("priv/proper", 0),
                  io:format("~p~n", [hello()]).
            
            hello() ->
                  "NIF library not loaded".
            ''')
        package = Package.from_path(project_dir)
        compiler = EnotCompiler(package)
        self.assertEqual(True, compiler.compile())
        self.assertEqual(True, os.path.exists(join(project_dir, 'priv/proper.so')))
        self.assertEqual(True, os.path.exists(join(project_dir, 'ebin/proper.beam')))
        p = subprocess.Popen(['erl', '-pa', 'ebin', '-run', 'proper', 'init', '-run', 'init', 'stop', '-noshell'],
                             stdout=PIPE,
                             cwd=project_dir)
        self.assertEqual(0, p.wait(5000))
        self.assertEqual('"Hello world!"\n', p.stdout.read().decode('utf8'))

    def test_error_project_compilation(self):
        create(self.test_dir, {'<name>': 'proper'})
        project_dir = join(self.test_dir, 'proper')
        ensure_dir(join(project_dir, 'c_src'))
        with open(join(project_dir, 'c_src/proper.c'), 'w') as w:
            w.write('''
            #include "erl_nif.h"

            static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
            {
                return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1)  // error here
            }
            
            static ErlNifFunc nif_funcs[] =
            {
                {"hello", 0, hello}
            };
            
            ERL_NIF_INIT(proper,nif_funcs,NULL,NULL,NULL,NULL)
            ''')
        with open(join(project_dir, 'src/proper.erl'), 'w') as w:
            w.write('''
            -module(proper).

            -export([init/0, hello/0]).
            
            init() ->
                  erlang:load_nif("priv/proper", 0),
                  io:format("~p~n", [hello()]).
            
            hello() ->
                  "NIF library not loaded".
            ''')
        package = Package.from_path(project_dir)
        compiler = EnotCompiler(package)
        self.assertEqual(False, compiler.compile())
        self.assertEqual(False, os.path.exists(join(project_dir, 'priv/proper.so')))
        self.assertEqual(False, os.path.exists(join(project_dir, 'ebin/proper.beam')))


if __name__ == '__main__':
    unittest.main()
