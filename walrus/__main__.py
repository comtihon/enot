"""Walrus - package manager for Erlang

Usage:
  walrus create <name>
  walrus build
  walrus package
  walrus add_package <repo> [-wp PACKAGE]
  walrus release
  walrus deps
  walrus version
  walrus -v | --version
  walrus -h | --help

Options:
  -p PACKAGE --package PACKAGE       path to package. If not specified, tries to add current application's package
  -w --rewrite                       should rewrite package if already loaded to repository with same version
                                     [default: False]
  -h --help                          show this help message and exit
  -v --version                       print version and exit
"""
import sys
from os.path import join

import walrus
import os
from docopt import docopt, DocoptExit
from walrus import APPVSN

from walrus.packages.package_builder import Builder
from pkg_resources import Requirement, resource_filename
from jinja2 import Template
from walrus.utils.file_utils import ensure_dir


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
    __ensure_template(src_dir, name, '.walrusfile.json')


# TODO upgrade deps support
# Build project with all deps (fetch deps if needed)
def build(path):
    builder = Builder.init_from_path(path)
    builder.populate()
    if not builder.build():
        sys.exit(1)
    else:
        sys.exit(0)


# Print project's version. Prefer walrus.conf vsn, but if none - use app.src version.
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
    builder.dep_packages()
    sys.exit(0)


# Create walrus package
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


def __ensure_template(src_dir, name, suffix):
    template = resource_filename(Requirement.parse(walrus.APPNAME), 'walrus/resources/template' + suffix)
    app_srcfile = join(src_dir, name + suffix)
    if not os.path.exists(app_srcfile):
        with open(join(src_dir, name + suffix), 'w') as f:
            f.write(Template(template).render(name=name))


if __name__ == "__main__":
    main()
