import sys

import enot
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
            if sys.version_info[0] == 3 and sys.version_info[1] > 4:
                continue
        if dep == 'pathlib':  # drop pathlib if on python 3.4+
            if sys.version_info[0] == 3 and sys.version_info[1] > 4:
                continue
        result.append(dep)
    return result


def get_name(dep: str) -> str:
    if '==' in dep:
        [name, _] = str.split(dep, '==')
        return name.strip()
    else:
        return dep.strip()


setup(name=enot.APPNAME,
      version=enot.APPVSN,
      description='Erlang package management and build system',
      author=enot.APPAUTHOR,
      author_email='valerii.tikhonov@gmail.com',
      url='https://github.com/comtihon/enot',
      packages=find_packages(),
      install_requires=get_requirements(),
      include_package_data=True,
      package_data={'enot': ['resources/*']},
      entry_points={
          'console_scripts': [
              'enot=enot.__main__:main'
          ]},
      classifiers=[
          'Programming Language :: Erlang',
          'License :: OSI Approved :: Apache Software License',
          'Programming Language :: Python :: 3.5',
          'Programming Language :: Python :: 3.6',
          'Programming Language :: Python :: 3.7',
          'Topic :: Software Development :: Build Tools'
      ]
      )
