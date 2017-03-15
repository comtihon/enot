import walrus
from setuptools import setup, find_packages

setup(name=walrus.APPNAME,
      version='1.0.0',
      description='Erlang package management and build system',
      author=walrus.APPAUTHOR,
      author_email='valerii.tikhonov@gmail.com',
      url='https://github.com/comtihon/walrus',
      packages=find_packages(),
      install_requires=['erl_terms', 'gitpython', 'artifactory', 'appdirs', 'boto'],
      include_package_data=True,
      package_data={'walrus': ['resources/global_config.json']},
      entry_points={
          'console_scripts': [
              'walrus=walrus.__main__:main'
          ]}
      )