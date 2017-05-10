import json
import unittest
from os.path import join

from jinja2 import Template

from coon.packages.package import Package
from coon.utils.file_utils import ensure_dir
from test.abs_test_class import TestClass


class DepsTests(TestClass):
    def __init__(self, method_name):
        super().__init__('deps_tests', method_name)

    @property
    def src_dir(self):
        return join(self.test_dir, 'src')

    @property
    def ebin_dir(self):
        return join(self.test_dir, 'ebin')

    # If there is a dep in package's config, but not in app - it should be added to package.apps
    def test_app_creating_new_dep(self):
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'test.app.src'), 'w') as w:
            w.write(get_application([]))
        with open(join(self.test_dir, 'coonfig.json'), 'w') as w:
            w.write(get_package_conf([{'name': 'test_dep',
                                       'url': "test_url",
                                       'vsn': "test_vsn"}]))
        package = Package.from_path(self.test_dir)
        self.assertEqual(['test_dep'], list(package.deps.keys()))  # test_dep in package deps (from package conf)
        self.assertEqual(['test_dep'], package.apps)  # test_dep in applications to be inserted in app (from deps)
        self.assertEqual([], package.config.applications)  # no applications in app.src

    def test_app_creating_new_app(self):
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'test.app.src'), 'w') as w:
            w.write(get_application(['mnesia']))
        with open(join(self.test_dir, 'coonfig.json'), 'w') as w:
            w.write(get_package_conf([]))
        package = Package.from_path(self.test_dir)
        self.assertEqual([], list(package.deps.keys()))  # no package deps
        self.assertEqual(["'mnesia'"], package.apps)  # mnesia in applications to be inserted in app (from apps)
        self.assertEqual(["'mnesia'"], package.config.applications)  # mnesia in package apps (from app.src conf)

    def test_app_creating_new_app_and_dep(self):
        ensure_dir(self.src_dir)
        with open(join(self.src_dir, 'test.app.src'), 'w') as w:
            w.write(get_application(['mnesia']))
        with open(join(self.test_dir, 'coonfig.json'), 'w') as w:
            w.write(get_package_conf([{'name': 'test_dep',
                                       'url': "test_url",
                                       'vsn': "test_vsn"}]))
        package = Package.from_path(self.test_dir)
        # TODO why here mnesia in single quotes like atom, and test_dep without atom quotes?
        self.assertEqual(['test_dep'], list(package.deps.keys()))  # test_dep in package deps (from package conf)
        self.assertEqual(["'mnesia'"], package.config.applications)  # mnesia in package apps (from app.src conf)
        apps = package.apps
        self.assertEqual(2, len(apps))  # mnesia and test_dep will go to app file
        self.assertEqual(True, 'test_dep' in apps)
        self.assertEqual(True, "'mnesia'" in apps)

    def test_app_creating_duplucates(self):
        True


def get_application(apps: list):
    app_str = '''
        {application, test,
          [
            {description, ""},
            {vsn, "1.0.0"},
            {registered, []},
            {modules, []},
            {applications, {{ apps }}},
            {mod, {test_app, []}},
            {env, []}
          ]}.
        '''
    return Template(app_str).render(apps=apps)


def get_package_conf(deps: list):
    conf_str = '''{
            \"name\":\"proper\",
            \"version\":\"1.0.0\",
            \"deps\": {{ deps }}
            }'''
    return Template(conf_str).render(deps=json.dumps(deps))


if __name__ == '__main__':
    unittest.main()
