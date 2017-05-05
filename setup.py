import sys

import coon
from setuptools import setup, find_packages


def get_requirements() -> list:
    with open('requirements.txt', 'r') as f:
        deps = f.readlines()
    result = []
    for dep in deps:
        if dep == '':  # drop empty deps
            continue
        dep = get_name(dep)
        if dep == 'enum34':  # drop enum34 if on python 3.4+
            if sys.version_info[1] > 4:
                continue
        result += dep
    return result


def get_name(dep: str) -> str:
    if '==' in dep:
        [name, _] = str.split(dep, '==')
        return name.strip()
    else:
        return dep.strip()


setup(name=coon.APPNAME,
      version=coon.APPVSN,
      description='Erlang package management and build system',
      author=coon.APPAUTHOR,
      author_email='valerii.tikhonov@gmail.com',
      url='https://github.com/comtihon/coon',
      packages=find_packages(),
      install_requires=get_requirements(),
      include_package_data=True,
      package_data={'coon': ['resources/*']},
      entry_points={
          'console_scripts': [
              'coon=coon.__main__:main'
          ]}
      )
