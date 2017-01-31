from setuptools import setup, find_packages

setup(name='walrus',
      version='1.0.0',
      description='Erlang package management and build system',
      author='Valerii Tikhonov',
      author_email='valerii.tikhonov@gmail.com',
      url='https://github.com/comtihon/walrus',
      packages=find_packages(), requires=['erl_terms', 'gitpython'],
      entry_points={
          'console_scripts': [
              'walrus=walrus.__main__:main'
          ]}
      )
