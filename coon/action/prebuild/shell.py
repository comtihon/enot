import subprocess

from coon.action.prebuild.action import Action


class Shell(Action):
    def __init__(self, params):
        super().__init__()
        self._params = params

    @property
    def params(self) -> str:
        return self._params

    def run(self, path):
        subprocess.run(self.params, check=True, shell=True, cwd=path)
