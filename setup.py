import walrus
from setuptools import setup, find_packages

setup(name=walrus.APPNAME,
      version=walrus.APPVSN,
      description='Erlang package management and build system',
      author=walrus.APPAUTHOR,
      author_email='valerii.tikhonov@gmail.com',
      url='https://github.com/comtihon/walrus',
      packages=find_packages(),
      install_requires=['erl_terms', 'docopt', 'gitpython', 'artifactory', 'appdirs', 'boto', 'Jinja2'],
      include_package_data=True,
      package_data={'walrus': ['resources/*']},
      entry_points={
          'console_scripts': [
              'walrus=walrus.__main__:main'
          ]}
      )
