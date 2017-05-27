from coon.packages.config.config import ConfigFile


class DepConfig(ConfigFile):
    def __init__(self, name: str, vsn: str, url: str):
        super().__init__()
        self._name = name
        self.set_url(url)
        self._git_vsn = vsn

    def get_compiler(self):
        RuntimeError("Dep " + self.name + "can't be compiled")
