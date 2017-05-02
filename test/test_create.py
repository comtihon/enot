import test
import unittest
from os.path import join

import os

import coon.__main__
from coon.packages.config.coon import CoonConfig
from coon.packages.package_builder import Builder
from coon.utils.erl_file_utils import get_value, get_values
from coon.utils.file_utils import read_file, ensure_empty, remove_dir

'''
Here are the tests, responsible for coon create
'''


class CreateTests(unittest.TestCase):
    test_name = 'create_tests'

    def setUp(self):
        ensure_empty(test.get_test_dir(self.test_name))

    def test_create(self):
        temp_dir = test.get_test_dir(self.test_name)
        coon.__main__.create(temp_dir, {'<name>': 'test_project'})
        project_dir = join(temp_dir, 'test_project')
        src_dir = join(project_dir, 'src')
        self.assertEqual(True, os.path.exists(project_dir))  # project dir was created
        self.assertEqual(True, os.path.exists(src_dir))  # src dir was created
        with open(join(project_dir, 'coonfig.json')) as config:
            file = config.read()
        config = CoonConfig(project_dir)
        self.assertEqual({}, config.init_from_json(file))  # no deps
        self.assertEqual('test_project', config.name)  # name was set and parsed properly
        self.assertEqual('0.0.1', config.vsn)  # version was set and parsed properly

    def test_compile_created(self):
        temp_dir = test.get_test_dir(self.test_name)
        project_dir = join(temp_dir, 'test_project')
        coon.__main__.create(temp_dir, {'<name>': 'test_project'})
        builder = Builder.init_from_path(project_dir)
        builder.populate()
        out_dir = join(project_dir, 'ebin')
        app_file = join(out_dir, 'test_project.app')
        self.assertEqual(True, builder.build())
        self.assertEqual(True, os.path.exists(join(out_dir, 'test_project_app.beam')))
        self.assertEqual(True, os.path.exists(join(out_dir, 'test_project_sup.beam')))
        self.assertEqual(True, os.path.exists(app_file))
        with open(app_file) as config:
            file = config.read()
        self.assertEqual("'test_project'", get_value('application', 0, file))
        self.assertEqual('"0.0.1"', get_value('vsn', 0, file))
        self.assertEqual(["'kernel'", "'stdlib'"], get_values('applications', file))
        modules = get_values('modules', file)
        self.assertEqual(2, len(modules))
        self.assertEqual(True, "'test_project_app'" in modules)
        self.assertEqual(True, "'test_project_sup'" in modules)

    def test_release_created(self):
        temp_dir = test.get_test_dir(self.test_name)
        project_dir = join(temp_dir, 'test_project')
        coon.__main__.create(temp_dir, {'<name>': 'test_project'})
        builder = Builder.init_from_path(project_dir)
        builder.populate()
        builder.build()
        self.assertEqual(True, builder.release())
        self.assertEqual(True, os.path.exists(join(project_dir, '_rel')))
        rel_dir = join(project_dir, '_rel', 'test_project')
        self.assertEqual(True, os.path.exists(rel_dir))
        self.assertEqual(True, os.path.exists(join(rel_dir, 'lib', 'test_project-0.0.1')))
        self.assertEqual(True, os.path.exists(join(rel_dir, 'releases', '0.0.1')))
        self.assertEqual(True, os.path.exists(join(project_dir, 'rel')))
        self.assertEqual(True, os.path.exists(join(project_dir, 'rel', 'sys.config')))
        self.assertEqual(True, os.path.exists(join(project_dir, 'rel', 'vm.args')))
        rel_content = read_file(join(project_dir, 'rel', 'vm.args'))
        rel_expected = '''-name {{ app.name }}@127.0.0.1
-setcookie {{ app.name }}'''
        self.assertEqual(rel_content.strip(), rel_expected)

    def tearDown(self):
        remove_dir(test.get_test_dir(self.test_name))


if __name__ == '__main__':
    unittest.main()
