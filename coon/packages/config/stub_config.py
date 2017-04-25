from coon.packages.config import ConfigFile


class StubConfig(ConfigFile):
    def __init__(self, name, vsn, compiler):
        super().__init__("")
        self._name = name
        self._app_vsn = vsn
        self._compiler = compiler

    def get_compiler(self):
        return self._compiler

    def read_config(self) -> dict:
        return {}
