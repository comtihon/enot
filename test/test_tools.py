import os
import unittest
from http.client import HTTPResponse
from os.path import join

from mock import patch

import enot.__main__
from enot.compiler.rebar import RebarCompiler
from enot.packages.package_builder import Builder
from enot.utils.file_utils import ensure_dir
from test.abs_test_class import TestClass


class ToolsTests(TestClass):
    def __init__(self, method_name):
        super().__init__('tools_tests', method_name)

    # Tool is installed in the system (or locally) - no need to install/add it to cache
    @patch('enot.utils.file_utils.ensure_programm')
    @patch('enot.global_properties.ensure_conf_file')
    def test_in_system(self, mock_conf, mock_get_cmd):
        mock_conf.return_value = self.conf_file
        mock_get_cmd.return_value = 'rebar'
        enot.__main__.create(self.test_dir, {'<name>': 'test'})
        project_dir = join(self.test_dir, 'test')
        builder = Builder.init_from_path(project_dir)
        compiler = RebarCompiler(builder.project)
        compiler.ensure_tool(builder.system_config.cache.local_cache)
        self.assertEqual(False, os.path.exists(join(self.tmp_dir, 'rebar')))
        self.assertEqual(False, os.path.exists(join(self.cache_dir, 'tool', 'rebar')))
        self.assertEqual(False, os.path.islink(join(self.test_dir, 'rebar')))
        self.assertEqual(False, builder.system_config.cache.local_cache.tool_exists('rebar'))

    # There is tool in cache. Should be linked to current project
    @patch('enot.utils.file_utils.ensure_programm')
    @patch('enot.global_properties.ensure_conf_file')
    def test_in_cache(self, mock_conf, mock_get_cmd):
        mock_conf.return_value = self.conf_file
        mock_get_cmd.return_value = False
        ensure_dir(join(self.cache_dir, 'tool'))
        with open(join(self.cache_dir, 'tool', 'rebar'), 'w') as outfile:  # 'load' tool to cache
            outfile.write('some content')
        enot.__main__.create(self.test_dir, {'<name>': 'test'})
        project_dir = join(self.test_dir, 'test')
        builder = Builder.init_from_path(project_dir)
        compiler = RebarCompiler(builder.project)
        compiler.ensure_tool(builder.system_config.cache.local_cache)
        self.assertEqual(True, os.path.islink(join(project_dir, 'rebar')))  # linked to current project
        self.assertEqual(True, builder.system_config.cache.local_cache.tool_exists('rebar'))  # and available in cache

    # There is no tool in the system, so it will be downloaded, added to cache and linked to current project
    @patch.object(HTTPResponse, 'read')
    @patch('enot.utils.file_utils.ensure_programm')
    @patch('enot.global_properties.ensure_conf_file')
    def test_missing(self, mock_conf, mock_get_cmd, mock_http_read):
        mock_conf.return_value = self.conf_file
        mock_get_cmd.return_value = False
        ensure_dir(self.tmp_dir)
        mock_http_read.return_value = b'some rebar binary content'
        enot.__main__.create(self.test_dir, {'<name>': 'test'})
        project_dir = join(self.test_dir, 'test')
        builder = Builder.init_from_path(project_dir)
        compiler = RebarCompiler(builder.project)
        compiler.ensure_tool(builder.system_config.cache.local_cache)
        self.assertEqual(True, os.path.exists(join(self.tmp_dir, 'rebar')))  # tool should be downloaded to tempdir
        self.assertEqual(True, os.path.exists(join(self.cache_dir, 'tool', 'rebar')))  # added to cache
        self.assertEqual(True, os.path.islink(join(project_dir, 'rebar')))  # linked to current project
        self.assertEqual(True, builder.system_config.cache.local_cache.tool_exists('rebar'))  # and available in cache


if __name__ == '__main__':
    unittest.main()
