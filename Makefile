make:
	python setup.py bdist_wheel

install: make
	sudo pip install dist/coon-*

tests:
	python -m pytest --capture=sys

.PHONY: install
