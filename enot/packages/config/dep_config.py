from enot.packages.config.config import ConfigFile
from enot.packages.dep import Dep


class DepConfig(ConfigFile):
    def __init__(self, name: str, dep: Dep):
        super().__init__()
        self._name = name
        self._url = dep.url
        self._git_tag = dep.tag
        self._git_branch = dep.branch

    def get_compiler(self):
        RuntimeError("Dep " + self.name + "can't be compiled")
