import os
import sys

from walrus.global_properties import WalrusGlobalProperties
from walrus.packages.package_builder import build_package


def main(args=None):
    if args is None:
        args = sys.argv[1:]
    if 'build' in args:
        return build()
    elif 'release' in args:
        return release()
    elif 'deps' in args:
        return deps()
    else:
        print('build | release | deps')
        sys.exit(1)
    # TODO walrusify?
    # TODO additional args


def build():
    system_config = WalrusGlobalProperties()
    if not build_package(os.getcwd(), system_config):
        sys.exit(1)
    else:
        sys.exit(0)


def release():
    return True


def deps():
    return True


if __name__ == "__main__":
    main()
