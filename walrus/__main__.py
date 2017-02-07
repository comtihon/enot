import os
import sys

from walrus.packages import package_builder
from walrus.global_properties import WalrusGlobalProperties
from walrus.packages.package_builder import build_package, Builder


def main(args=None):
    if args is None:
        args = sys.argv[1:]
    if 'build' in args:
        return build()
    elif 'walrusify' in args:
        return walrusify(os.getcwd())  # TODO path
    elif 'release' in args:
        return release()
    elif 'deps' in args:
        return deps(os.getcwd())
    else:
        print('build | release | deps | walrusify')
        sys.exit(1)
        # TODO additional args


def build():
    system_config = WalrusGlobalProperties()
    if not build_package(os.getcwd(), system_config):
        sys.exit(1)
    else:
        sys.exit(0)


def release():
    return True


def deps(path):
    builder = Builder(path)
    if builder.populate():
        sys.exit(0)
    else:
        sys.exit(1)


def walrusify(path):
    if package_builder.walrusify(path):
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
