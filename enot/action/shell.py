import subprocess

from enot.action.action import Action


class Shell(Action):
    def __init__(self, params):
        super().__init__()
        self._params = params

    @property
    def params(self) -> str:
        return self._params

    def run(self, path, package=None, system_config=None, erlang_vsn: str = None) -> bool:
        try:
            subprocess.check_call(self.params, shell=True, cwd=path)
            return True
        except subprocess.CalledProcessError:
            return False

    def export(self) -> dict:
        return {'shell': self.params}
