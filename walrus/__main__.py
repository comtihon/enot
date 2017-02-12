import os
import sys

from walrus.packages import package_builder
from walrus.packages.package_builder import Builder


def main(args=None):
    if args is None:
        args = sys.argv[1:]
    if 'build' in args:
        return build(os.getcwd())
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


def build(path):
    builder = Builder(path)
    package = builder.populate()
    if not builder.build_tree(package):
        sys.exit(1)
    else:
        sys.exit(0)


def release():
    return True


def deps(path):
    builder = Builder(path)
    package = builder.populate()    # TODO link deps to package
    for dep in package.list_deps():
        if not builder.build_tree(dep):
            sys.exit(1)
    sys.exit(0)


def walrusify(path):
    if package_builder.walrusify(path):
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
