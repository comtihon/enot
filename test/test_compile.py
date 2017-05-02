import unittest
from os.path import join

from mock import patch

import test
from coon.compiler.coon import CoonCompiler
from coon.packages.config.coon import CoonConfig
from coon.packages.package import Package
from coon.utils.file_utils import ensure_empty, remove_dir, ensure_dir


class CompileTests(unittest.TestCase):
    test_name = 'create_tests'

    def setUp(self):
        ensure_empty(test.get_test_dir(self.test_name))

    @patch.object(CoonCompiler, '_CoonCompiler__write_app_file')
    @patch.object(CoonConfig, 'read_config')
    def test_porper_compilation(self, mock_config, mock_compiler):
        mock_config.return_value = {}
        mock_compiler.return_value = True
        temp_dir = test.get_test_dir(self.test_name)
        src_dir = join(temp_dir, 'src')
        ensure_dir(src_dir)
        with open(join(src_dir, 'proper.erl'), 'w') as w:
            w.write('''
            -module(proper).
            -export([test/0]).
            test() -> do_smth(1).
            do_smth(A) -> A + 1.
            ''')
        config = CoonConfig(temp_dir)
        config.init_from_dict({'name': 'test'})
        package = Package(config=config)
        compiler = CoonCompiler(package)
        self.assertEqual(True, compiler.compile())

    def tearDown(self):
        remove_dir(test.get_test_dir(self.test_name))


if __name__ == '__main__':
    unittest.main()
