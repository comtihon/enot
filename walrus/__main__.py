"""Walrus - package manager for Erlang

Usage:
  walrus build
  walrus package
  walrus add_package <repo> [-wp PACKAGE]
  walrus deps
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

import os
from docopt import docopt, DocoptExit
from walrus import APPVSN

from walrus.packages.package_builder import Builder


def main(args=None):
    try:
        arguments = docopt(__doc__, argv=args, version=APPVSN)
    except DocoptExit as usage:
        print(usage)
        sys.exit(1)
    path = os.getcwd()
    if arguments['build']:
        return build(path)
    if arguments['deps']:
        return deps(path)
    if arguments['package']:
        return package(path)
    if arguments['add_package']:
        return add_package(path, arguments)


# TODO release support
# Build project with all deps (fetch deps if needed)
def build(path):
    builder = Builder.init_from_path(path)
    builder.populate()
    if not builder.build():
        sys.exit(1)
    else:
        sys.exit(0)


def release():
    return True


#  TODO add an ability to link full deps tree to project
# Fetch and build deps
def deps(path):
    builder = Builder.init_from_path(path)
    builder.populate()
    builder.deps()
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


if __name__ == "__main__":
    main()
