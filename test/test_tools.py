import json
import os
import unittest
from os.path import join
from mock import patch

import test
from http.client import HTTPResponse
from coon.packages.package import Package
from coon.packages.package_builder import Builder
from coon.utils.file_utils import ensure_empty, remove_dir, ensure_dir
from coon.packages.config.stub_config import StubConfig


class ToolsTests(unittest.TestCase):
    test_name = 'tools_tests'

    @property
    def test_dir(self):
        return test.get_test_dir(self.test_name)

    @property
    def cache_dir(self):
        return join(self.test_dir, 'cache')

    @property
    def tmp_dir(self):
        return join(self.test_dir, 'tmp')

    @property
    def conf_path(self):
        return join(self.test_dir, 'global_config.json')

    def setUp(self):
        ensure_empty(test.get_test_dir(self.test_name))
        conf = {'temp_dir': self.tmp_dir,
                'compiler': 'rebar',
                'cache': [
                    {
                        'name': 'local_cache',
                        'type': 'local',
                        'url': 'file://' + self.cache_dir
                    }]}
        with open(self.conf_path, 'w') as outfile:
            json.dump(conf, outfile, sort_keys=True, indent=4)

    # Tool is installed in the system (or locally) - no need to install/add it to cache
    @patch('coon.utils.file_utils.get_cmd')
    def test_in_system(self, mock_get_cmd):
        mock_get_cmd.return_value = "rebar"
        builder = Builder(self.test_dir, Package(StubConfig('test', None, path=self.test_dir)))
        self.assertEqual(False, os.path.exists(join(self.tmp_dir, 'rebar')))
        self.assertEqual(False, os.path.exists(join(self.cache_dir, 'tool', 'rebar')))
        self.assertEqual(False, os.path.islink(join(self.test_dir, 'rebar')))
        self.assertEqual(False, builder.system_config.cache.local_cache.tool_exists('rebar'))

    # There is tool in cache. Should be linked to current project
    @patch('coon.utils.file_utils.get_cmd')
    @patch('coon.global_properties.ensure_conf_file')
    def test_in_cache(self, mock_conf, mock_get_cmd):
        mock_conf.return_value = self.conf_path
        mock_get_cmd.return_value = None
        ensure_dir(join(self.cache_dir, 'tool'))
        with open(join(self.cache_dir, 'tool', 'rebar'), 'w') as outfile:  # 'load' tool to cache
            outfile.write('some content')
        builder = Builder(self.test_dir, Package(StubConfig('test', None, path=self.test_dir)))
        self.assertEqual(True, os.path.islink(join(self.test_dir, 'rebar')))  # linked to current project
        self.assertEqual(True, builder.system_config.cache.local_cache.tool_exists('rebar'))  # and available in cache

    # There is no tool in the system, so it will be downloaded, added to cache and linked to current project
    @patch.object(HTTPResponse, 'read')
    @patch('coon.utils.file_utils.get_cmd')
    @patch('coon.global_properties.ensure_conf_file')
    def test_missing(self, mock_conf, mock_get_cmd, mock_http_read):
        mock_conf.return_value = self.conf_path
        mock_get_cmd.return_value = None
        ensure_dir(self.tmp_dir)
        mock_http_read.return_value = b'some rebar binary content'
        builder = Builder(self.test_dir, Package(StubConfig('test', None, path=self.test_dir)))
        self.assertEqual(True, os.path.exists(join(self.tmp_dir, 'rebar')))  # tool should be downloaded to tempdir
        self.assertEqual(True, os.path.exists(join(self.cache_dir, 'tool', 'rebar')))  # added to cache
        self.assertEqual(True, os.path.islink(join(self.test_dir, 'rebar')))  # linked to current project
        self.assertEqual(True, builder.system_config.cache.local_cache.tool_exists('rebar'))  # and available in cache

    def tearDown(self):
        remove_dir(test.get_test_dir(self.test_name))

if __name__ == '__main__':
    unittest.main()
