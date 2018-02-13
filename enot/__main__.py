"""Enot - package manager for Erlang

Usage:
  enot create <name> [-l LEVEL]
  enot build [-l LEVEL][--define VARLINE]
  enot package [-l LEVEL][--define VARLINE]
  enot release [-l LEVEL][--define VARLINE]
  enot fetch <package> [<version>] [-l LEVEL]
  enot install <package> [<version>] [-l LEVEL]
  enot uninstall <package> [-l LEVEL]
  enot installed
  enot deps [-l LEVEL]
  enot version
  enot upgrade [-d DEP] [-l LEVEL]
  enot eunit [-l LEVEL][--define VARLINE]
  enot ct [--log-dir DIR] [-l LEVEL][--define VARLINE]
  enot -v | --version
  enot -h | --help

Options:
  -h --help                          show this help message and exit
  -v --version                       print Enot's version and exit
  -l LEVEL --log-level LEVEL         set log level. Options: debug, info, warning, error, critical [default: info]
  --log-dir DIR                      common tests log dir [default: test/logs]
  -d DEP --dep DEP                   ignore lock only for certain dep.
  --define VARLINE                   define vars for file compilation. Used in erlang preprocessor. different vars
                                     should be separated with spaces, KV vars should use, f.e. --define 'TEST VAR=123'.
                                     [default: '']
"""
import os
import sys
from os.path import join

from docopt import docopt, DocoptExit
from jinja2 import Template
from pkg_resources import Requirement, resource_filename

import enot
from enot import APPVSN
from enot.packages.package_builder import Builder
from enot.packages.package_controller import Controller
from enot.utils import logger
from enot.utils.file_utils import ensure_dir
from enot.utils.logger import warning


def main(args=None):
    try:
        arguments = docopt(__doc__, argv=args, version=APPVSN)
    except DocoptExit as usage:
        print(usage)
        sys.exit(1)
    path = os.getcwd()
    logger.configure(arguments['--log-level'])
    result = False
    if arguments['create']:
        result = create(path, arguments)
    if arguments['build']:
        result = build(path, arguments)
    if arguments['version']:
        result = version(path)
    if arguments['deps']:
        result = deps(path)
    if arguments['release']:
        result = release(path, arguments)
    if arguments['package']:
        result = package(path, arguments)
    if arguments['upgrade']:
        result = upgrade(path, arguments)
    if arguments['eunit']:
        result = eunit(path, arguments)
    if arguments['ct']:
        result = ct(path, arguments)
    if arguments['fetch']:
        result = fetch(arguments)
    if arguments['install']:
        result = install(arguments)
    if arguments['uninstall']:
        result = uninstall(arguments)
    if arguments['installed']:
        result = installed()
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
    __ensure_template(project_dir, name, 'enot_config.json', True)
    return True


# Build project with all deps (fetch deps if needed)
def build(path, arguments: dict):
    define = arguments['--define']
    builder = Builder.init_from_path(path)
    return do_build(builder, define)


def do_build(builder: Builder, define: str, test=False):
    builder.populate(test)
    return builder.build(define)


# Print project's application version. Prefer enot_config.json vsn, but if none - use app.src version.
def version(path):
    builder = Builder.init_from_path(path)
    print(builder.project.vsn)  # TODO return vsn?
    return True


# Build a release. Will use current rel dir with config or create new, if none is found
def release(path, arguments: dict):
    define = arguments['--define']
    builder = Builder.init_from_path(path)
    if not do_build(builder, define):  # TODO check if project was already built
        return False
    builder.release()
    return True


# Fetch and build deps
def deps(path):
    builder = Builder.init_from_path(path)
    builder.populate()
    builder.deps()
    return True


# Create enot package
def package(path, arguments: dict):
    define = arguments.get('--define', '')
    builder = Builder.init_from_path(path)
    if not do_build(builder, define):
        return False
    builder.package()
    return True


# Run upgrade
def upgrade(path, arguments):
    dep = arguments.get('--dep', None)
    builder = Builder.init_from_path(path)
    builder.drop_locs(dep)
    builder.populate()
    return True


# Fetch package to local cache
def fetch(arguments):
    fullname = __get_full_name(arguments)
    maybe_vsn = arguments['<version>']
    controller = Controller()
    return controller.fetch(fullname, maybe_vsn)


# Run package installation steps. If not in local cache - fetch it.
def install(arguments):
    fullname = __get_full_name(arguments)
    maybe_vsn = arguments['<version>']
    controller = Controller()
    return controller.install(fullname, maybe_vsn)


# Uninstall previously installed package. It still remains in local cache.
def uninstall(arguments):
    fullname = __get_full_name(arguments)
    controller = Controller()
    return controller.uninstall(fullname)


# Print installed packages
def installed():
    print(Controller().installed())
    return True


# Run tests
def eunit(path, arguments: dict):
    define = arguments['--define']
    builder = Builder.init_from_path(path)
    if not do_build(builder, define, test=True):
        return False
    return builder.unit_test()


def ct(path, arguments):
    log_dir = arguments['--log-dir']
    define = arguments['--define']
    builder = Builder.init_from_path(path)
    if not do_build(builder, define, test=True):
        return False
    return builder.common_test(log_dir)


def __ensure_template(src_dir, name, suffix, overwrite_name=False):
    template = resource_filename(Requirement.parse(enot.APPNAME), 'enot/resources/template' + suffix)
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


def __get_full_name(args: dict) -> str:
    fullname = args['<package>']
    if fullname is None or '/' not in fullname:
        warning('Incorrect package parameter. Should be namespace/package_name.')
        raise ValueError('Incorrect package parameter\'s value')
    return fullname


if __name__ == "__main__":
    main()
