"""Coon - package manager for Erlang

Usage:
  coon create <name>
  coon build
  coon package
  coon add_package <repo> [-wp PACKAGE] [-r RECURSE]
  coon release
  coon deps
  coon version
  coon -v | --version
  coon -h | --help

Options:
  -p PACKAGE --package PACKAGE       path to package. If not specified, tries to add current application's package
  -w --rewrite                       should rewrite package if already loaded to repository with same version
                                     [default: True]
  -r RECURSE --recurse RECURSE       add package and all it's deps recursively [default: True]
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
    result = False
    if arguments['create']:
        result = create(path, arguments)
    if arguments['build']:
        result = build(path)
    if arguments['version']:
        result = version(path)
    if arguments['deps']:
        result = deps(path)
    if arguments['release']:
        result = release(path)
    if arguments['package']:
        result = package(path)
    if arguments['add_package']:
        result = add_package(path, arguments)
    if result:
        sys.exit(0)
    else:
        sys.exit(1)


def create(path: str, arguments: dict):
    name = arguments['<name>']
    project_dir = join(path, name)
    src_dir = join(project_dir, 'src')
    ensure_dir(project_dir)
    ensure_dir(src_dir)
    __ensure_template(src_dir, name, '_app.erl')
    __ensure_template(src_dir, name, '_sup.erl')
    __ensure_template(src_dir, name, '.app.src')
    __ensure_template(project_dir, name, 'coonfig.json', True)
    return True


# TODO upgrade deps support
# Build project with all deps (fetch deps if needed)
def build(path):
    builder = Builder.init_from_path(path)
    return do_build(builder)


def do_build(builder):
    builder.populate()
    return builder.build()


# Print project's version. Prefer coonfing.json vsn, but if none - use app.src version.
def version(path):
    builder = Builder.init_from_path(path)
    print(builder.project.vsn)
    return True


# Build a release. Will use current rel dir with config or create new, if none is found
def release(path):
    builder = Builder.init_from_path(path)
    if not do_build(builder):
        return False
    builder.release()
    return True


# Fetch and build deps
def deps(path):
    builder = Builder.init_from_path(path)
    builder.populate()
    builder.deps()
    return True


# Create coon package
def package(path):
    builder = Builder.init_from_path(path)
    if not do_build(builder):
        return False
    builder.package()
    return True


# Add created package to cache
def add_package(path, arguments):
    repo = arguments['<repo>']
    rewrite = arguments['--rewrite']
    recurse = arguments['--recurse']
    path_overwrite = arguments.get('--package', False)
    if path_overwrite:
        builder = Builder.init_from_package(path_overwrite)
    else:
        builder = Builder.init_from_path(path)
    return builder.add_package(repo, rewrite, recurse)


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
                                             apps_tmp="{{ app.std_deps + app.deps }}",
                                             modules_tmp="{{ modules }}"))


if __name__ == "__main__":
    main()
