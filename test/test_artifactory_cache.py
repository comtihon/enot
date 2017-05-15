import unittest
from os.path import join

import os
from artifactory import ArtifactoryPath
from mock import patch

from coon.__main__ import create, package
from coon.packages.package import Package
from coon.packages.package_builder import Builder
from test.abs_test_class import TestClass


# TODO make this testcase run only if artifactory is available locally
class ArtifactoryTests(TestClass):
    def __init__(self, method_name):
        super().__init__('artifactory_tests', method_name)

    @property
    def path(self):
        return 'http://localhost:8081/artifactory/generic-local'

    @property
    def username(self):
        return 'publisher'

    @property
    def password(self):  # TODO change password before push!!!
        return 'ytjnlfvgfc'

    @property
    def global_config(self):
        return {'temp_dir': self.tmp_dir,
                'compiler': 'coon',
                'cache': [
                    {
                        'name': 'local_cache',
                        'type': 'local',
                        'url': 'file://' + self.cache_dir
                    },
                    {
                        'name': 'artifactory-local',
                        'type': 'artifactory',
                        'url': self.path,
                        'username': self.username,
                        'password': self.password
                    }]}

    def setUp(self):
        super().setUp()
        create(self.test_dir, {'<name>': 'test_project'})

    # Test package uploading to artifactory
    @patch('coon.global_properties.ensure_conf_file')
    def test_simple_uploading(self, mock_conf):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_project')
        pack = Package.from_path(pack_path)
        package(pack_path)
        builder = Builder.init_from_path(pack_path)
        builder.system_config.cache.add_package(pack, 'artifactory-local', False, False)
        exists = ArtifactoryTests.check_exists(builder.system_config.cache.caches, pack)
        self.assertEqual(True, exists)

    # check if not exists, add package, check if exists
    @patch('coon.global_properties.ensure_conf_file')
    def test_exists(self, mock_conf):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_project')
        pack = Package.from_path(pack_path)
        builder = Builder.init_from_path(pack_path)
        exists = ArtifactoryTests.check_exists(builder.system_config.cache.caches, pack)
        self.assertEqual(False, exists)
        package(pack_path)
        self.assertEqual(True, os.path.isfile(join(pack_path, 'test_project.cp')))
        builder.system_config.cache.add_package(pack, 'artifactory-local', False, False)
        exists = ArtifactoryTests.check_exists(builder.system_config.cache.caches, pack)
        self.assertEqual(True, exists)

    # download package from remote cache, add to local
    @patch('coon.global_properties.ensure_conf_file')
    def test_simple_downloading(self, mock_conf):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_project')
        pack = Package.from_deps('test_project', ('git://github.com/comtihon/test_project', '0.0.1'))
        pack.fill_from_path(pack_path)
        package(pack_path)
        builder = Builder.init_from_path(pack_path)
        builder.system_config.cache.add_package(pack, 'artifactory-local', False, False)
        self.assertEqual(False, builder.system_config.cache.local_cache.exists(pack))
        artifactory_cache = builder.system_config.cache.caches['artifactory-local']
        artifactory_cache.fetch_package(pack)
        self.assertEqual(True, os.path.isfile(join(self.tmp_dir, 'test_project.cp')))
        builder.system_config.cache.add_fetched(artifactory_cache, pack)
        self.assertEqual(True, builder.system_config.cache.local_cache.exists(pack))

    def test_uploading_with_deps(self):
        True  # TODO emulate project with multiple deps. Deps should be uploaded to cache recursively

    def test_downloading_with_deps(self):
        True  # TODO emulate project with multiple deps. Deps should be downloaded to local cache resursively

    def tearDown(self):
        super().tearDown()
        path = ArtifactoryPath(self.path + '/' + self.username, auth=(self.username, self.password))
        if path.exists():
            path.rmdir()

    @staticmethod
    def check_exists(caches: dict, package: Package):
        for cache in caches.values():
            if cache.exists(package):
                return True
        return False


if __name__ == '__main__':
    unittest.main()
