PROJECT = walrus
PROJECT_VERSION = 1.0.0

make:
	python setup.py bdist_wheel

install: make
	sudo pip install dist/walrus-1.0.0-py3-none-any.whl

.PHONY: install
