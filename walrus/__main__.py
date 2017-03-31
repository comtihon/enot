import sys

import os

from walrus.packages import package_builder
from walrus.packages.package_builder import Builder


def main(args=None):
    if args is None:
        args = sys.argv[1:]
    if 'build' in args:
        return build(os.getcwd())
    elif 'walrusify' in args:
        return walrusify(os.getcwd())
    elif 'release' in args:
        return release()
    elif 'package' in args:
        return package(os.getcwd())
    elif 'deps' in args:
        return deps(os.getcwd())
    else:
        print('build | release | deps | walrusify')
        sys.exit(1)
        # TODO additional args


# TODO release support
def build(path):
    builder = Builder(path)
    builder.populate()
    if not builder.build():
        sys.exit(1)
    else:
        sys.exit(0)


def release():
    return True


#  TODO add an ability to link full deps tree to project
def deps(path):
    builder = Builder(path)
    builder.populate()
    builder.deps()
    sys.exit(0)


def package(path):
    builder = Builder(path)
    builder.populate()
    builder.package()


def walrusify(path):
    if package_builder.walrusify(path):
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
