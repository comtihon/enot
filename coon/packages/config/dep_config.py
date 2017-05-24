from coon.packages.config.config import ConfigFile


class DepConfig(ConfigFile):
    def __init__(self, name: str, vsn: str, url: str):
        super().__init__(vsn=vsn, url=url)
        self._name = name

    def get_compiler(self):
        RuntimeError("Dep " + self.name + "can't be compiled")
