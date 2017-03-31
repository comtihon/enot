from walrus.packages.config import ConfigFile


class StubConfig(ConfigFile):
    def __init__(self, name, vsn):
        super().__init__("")
        self._name = name
        self._app_vsn = vsn

    def get_compiler(self):
        return None

    def read_config(self) -> dict:
        return {}
