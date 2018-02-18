import unittest
from os.path import join

import requests_mock

from enot.__main__ import create
from enot.pac_cache.enot_cache import EnotCache
from test.abs_test_class import TestClass, set_git_url, set_git_tag

OK_1_19_2_20 = '{"result":true,"response":[{"ref":"1","erl_version":"19"},{"ref":"2","erl_version":"19"}]}'
OK_1_18_1_19 = '{"result":true,"response":[{"ref":"1","erl_version":"18"},{"ref":"1","erl_version":"19"}]}'
OK_BUILD_1 = '{"result":true,"response":' \
             '[{"build_id":"1","result":true,"message":"","artifactPath":"path","createdDate":"date"}]}'


class EnotCacheTests(TestClass):
    def __init__(self, method_name):
        super().__init__('enot_cache_tests', method_name)

    @property
    def path(self):
        return 'http://localhost:8080'

    @property
    def conf(self):
        return {
            'name': 'remote',
            'type': 'enot',
            'url': self.path
        }

    @property
    def global_config(self):
        return {'temp_dir': self.tmp_dir,
                'compiler': self.compiler,
                'cache': [
                    {
                        'name': 'local_cache',
                        'type': 'local',
                        'url': 'file://' + self.cache_dir
                    },
                    self.conf
                ]}

    def setUp(self):
        super().setUp()
        create(self.test_dir, {'<name>': 'test_project'})
        pack_path = join(self.test_dir, 'test_project')
        set_git_url(pack_path, 'http://github/comtihon/test_app')
        set_git_tag(pack_path, '1.0.0')

    @requests_mock.mock()
    def test_versions(self, mock_post):
        mock_post.post(join(self.path, 'versions'), text=OK_1_19_2_20)
        cache = EnotCache(self.test_dir, '20', self.conf)
        versions = cache.get_versions('test')
        self.assertEqual(['1', '2'], versions)

    @requests_mock.mock()
    def test_erl_versions(self, mock_post):
        mock_post.post(join(self.path, 'versions'), text=OK_1_18_1_19)
        cache = EnotCache(self.test_dir, '20', self.conf)
        versions = cache.get_erl_versions('test', '1')
        self.assertEqual(['18', '19'], versions)


if __name__ == '__main__':
    unittest.main()
