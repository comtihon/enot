import coon
from setuptools import setup, find_packages

setup(name=coon.APPNAME,
      version=coon.APPVSN,
      description='Erlang package management and build system',
      author=coon.APPAUTHOR,
      author_email='valerii.tikhonov@gmail.com',
      url='https://github.com/comtihon/coon',
      packages=find_packages(),
      install_requires=['erl_terms', 'docopt', 'gitpython', 'artifactory', 'appdirs', 'boto', 'Jinja2'],
      include_package_data=True,
      package_data={'coon': ['resources/*']},
      entry_points={
          'console_scripts': [
              'coon=coon.__main__:main'
          ]}
      )
