"""
Application config .app.src or .app
"""
from logging import warning
from os.path import join
from tarfile import TarFile

from enot.utils.erl_file_utils import parse_app_config, contains_app_file, parse_app_config_content


class AppConfig:
    def __init__(self, name: str, vsn: str, apps: list, template: bool, compose=True) -> None:
        self._name = name
        self._vsn = vsn
        self._applications = apps
        self._is_template = template
        self._compose_app_file = compose

    @classmethod
    # Find and parse src/Name.app.src. If none - find and parse ebin/Name.app. If none - return None
    def from_path(cls, path: str) -> 'AppConfig' or None:
        src_path = join(path, 'src')
        if contains_app_file(src_path):  # Check .app.src
            name, vsn, apps, template = parse_app_config(src_path)
            return cls(name, vsn, apps, template)
        ebin_path = join(path, 'ebin')  # Check .app
        if contains_app_file(ebin_path, suffix='.app'):
            name, vsn, apps, template = parse_app_config(ebin_path, suffix='.app')
            return cls(name, vsn, apps, template, False)
        warning('No app in path ' + path)
        return None

    @classmethod
    def from_package(cls, file: str, package: TarFile, compose=False):
        f = package.extractfile(file)
        (name, vsn, apps, is_template) = parse_app_config_content(f.read())
        # when calling from content - project was already built. No need to compose app file.
        return cls(name, vsn, apps, is_template, compose=compose)

    @property
    def name(self) -> str:
        return self._name

    @property
    def applications(self) -> list:
        return self._applications

    @property
    def vsn(self) -> str:
        return self._vsn

    @property
    def is_template(self) -> bool:  # If app config contains jinja2 templates.
        return self._is_template

    @property
    def compose_app_file(self) -> bool:  # If there is a need to create ebin/<Name>.app during compilation
        return self._compose_app_file
