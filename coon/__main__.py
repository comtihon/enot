"""Coon - package manager for Erlang

Usage:
  coon create <name>
  coon build
  coon package
  coon add_package <repo> [-wp PACKAGE]
  coon release
  coon deps
  coon version
  coon -v | --version
  coon -h | --help

Options:
  -p PACKAGE --package PACKAGE       path to package. If not specified, tries to add current application's package
  -w --rewrite                       should rewrite package if already loaded to repository with same version
                                     [default: False]
  -h --help                          show this help message and exit
  -v --version                       print version and exit
"""
import sys
from os.path import join

import coon
import os
from coon import APPVSN
from docopt import docopt, DocoptExit
from jinja2 import Template
from pkg_resources import Requirement, resource_filename

from coon.packages.package_builder import Builder
from coon.utils.file_utils import ensure_dir


def main(args=None):
    try:
        arguments = docopt(__doc__, argv=args, version=APPVSN)
    except DocoptExit as usage:
        print(usage)
        sys.exit(1)
    path = os.getcwd()
    if arguments['create']:
        return create(path, arguments)
    if arguments['build']:
        return build(path)
    if arguments['version']:
        return version(path)
    if arguments['deps']:
        return deps(path)
    if arguments['release']:
        return release(path)
    if arguments['package']:
        return package(path)
    if arguments['add_package']:
        return add_package(path, arguments)


def create(path, arguments):
    name = arguments['<name>']
    project_dir = join(path, name)
    src_dir = join(project_dir, 'src')
    ensure_dir(project_dir)
    ensure_dir(src_dir)
    __ensure_template(src_dir, name, '_app.erl')
    __ensure_template(src_dir, name, '_sup.erl')
    __ensure_template(src_dir, name, '.app.src')
    __ensure_template(project_dir, name, 'coonfig.json', True)


# TODO upgrade deps support
# Build project with all deps (fetch deps if needed)
def build(path):
    builder = Builder.init_from_path(path)
    builder.populate()
    if not builder.build():
        sys.exit(1)
    else:
        sys.exit(0)


# Print project's version. Prefer coonfing.json vsn, but if none - use app.src version.
def version(path):
    builder = Builder.init_from_path(path)
    print(builder.project.vsn)
    sys.exit(0)


# Build a release. Will use current rel dir with config or create new, if none is found
def release(path):
    builder = Builder.init_from_path(path)
    builder.release()
    sys.exit(0)


#  TODO add an ability to link full deps tree to project
# Fetch and build deps
def deps(path):
    builder = Builder.init_from_path(path)
    builder.populate()
    builder.deps()
    sys.exit(0)


# Create coon package
def package(path):
    builder = Builder.init_from_path(path)
    builder.populate()
    builder.package()
    sys.exit(0)  # TODO use sys.exit only in main. All sub functions should return bool()


# Add package to cache
def add_package(path, arguments):
    repo = arguments['<repo>']
    rewrite = arguments['--rewrite']
    path_overwrite = arguments.get('--package', False)
    if path_overwrite:
        builder = Builder.init_from_package(path_overwrite)
    else:
        builder = Builder.init_from_path(path)
    builder.add_package(repo, rewrite)
    sys.exit(0)


def __ensure_template(src_dir, name, suffix, overwrite_name=False):
    template = resource_filename(Requirement.parse(coon.APPNAME), 'coon/resources/template' + suffix)
    if overwrite_name:
        filename = suffix
    else:
        filename = name + suffix
    app_srcfile = join(src_dir, filename)
    with open(template, 'r') as r:
        content = r.read()
    if not os.path.exists(app_srcfile):
        with open(app_srcfile, 'w') as f:
            f.write(Template(content).render(name=name,
                                             vsn_tmp="{{ app.vsn }}",
                                             apps_tmp="{{ app.std_apps + app.apps }}",
                                             modules_tmp="{{ modules }}"))


if __name__ == "__main__":
    main()
