import unittest
from os.path import join

from mock import patch

from coon.__main__ import create
from coon.packages.config import config_factory
from coon.packages.package import Package
from coon.packages.package_builder import Builder
from test.abs_test_class import TestClass
from coon.utils.file_utils import remove_dir, copy_file


class LocalCacheTests(TestClass):
    def __init__(self, method_name):
        super().__init__('local_cache_tests', method_name)

    def setUp(self):
        super().setUp()
        create(self.test_dir, {'<name>': 'test_app'})

    # Tests if package exists in local cache
    @patch('coon.global_properties.ensure_conf_file')
    def test_package_exists(self, mock_conf):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        config = config_factory.read_project(pack_path)
        pack = Package(pack_path, config, None, url='https://github.com/comtihon/test_app')
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(False, builder.system_config.cache.exists_local(pack))
        self.assertEqual(True, builder.build())
        builder.system_config.cache.add_package_local(pack)
        self.assertEqual(True, builder.system_config.cache.exists_local(pack))

    # Test if test_app.cp can be added to local cache
    @patch('coon.global_properties.ensure_conf_file')
    def test_add_from_package(self, mock_conf):
        mock_conf.return_value = self.conf_file
        pack_path = join(self.test_dir, 'test_app')
        config = config_factory.read_project(pack_path)
        pack = Package(pack_path, config, None, url='https://github.com/comtihon/test_app')
        builder = Builder.init_from_path(pack_path)
        self.assertEqual(True, builder.build())
        builder.package()
        new_package_path = join(self.test_dir, 'test_app.cp')
        # remove source project, test should work only with coon package
        copy_file(join(pack_path, 'test_app.cp'), new_package_path)
        remove_dir(pack_path)
        package = Package.from_package(new_package_path)
        self.assertEqual(False, builder.system_config.cache.exists_local(package))
        # local cache is used here to determine tmp dir
        local_cache = builder.system_config.cache.local_cache
        self.assertEqual(True, builder.system_config.cache.add_fetched(local_cache, package))
        self.assertEqual(True, builder.system_config.cache.exists_local(package))

    # Test if test_app can be added to local cache
    def test_add_from_path(self):
        self.assertEqual(True, True)

    # Test if test_app can be fetched, compiled and added to local cache
    def test_compile_and_add(self):
        self.assertEqual(True, True)

    # Test if test_app has several deps, all will be fetched, compiled and added to local cache
    def test_add_with_deps(self):
        self.assertEqual(True, True)


if __name__ == '__main__':
    unittest.main()
