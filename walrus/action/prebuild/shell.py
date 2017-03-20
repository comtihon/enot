import subprocess

from walrus.action.prebuild.action import Action


class Shell(Action):
    params = ""

    def __init__(self, params):
        super().__init__()
        self.params = params

    def run(self, path):
        subprocess.run(self.params, check=True, shell=True, cwd=path)
