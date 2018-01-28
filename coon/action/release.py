import shutil
import subprocess
from os import getcwd, remove

from os.path import join

from coon.action.action import Action
from coon.compiler.relx import RelxCompiler
from coon.utils.file_utils import untar
from coon.utils.logger import error


class Release(Action):
    def __init__(self, params: dict):
        super().__init__()
        self._params = params

    @property
    def params(self) -> dict:
        return self._params

    def run(self, path, package=None, system_config=None) -> bool:
        coon_cache = system_config.cache.official_cache
        erts_tarball = None
        try:
            erts_tarball = self.__download_erts(coon_cache, package.path)
            compiler = RelxCompiler(package)
            compiler.ensure_tool(system_config.cache.local_cache)
            compiler.compile(params=['-i', package.path])
            shutil.copytree(join(package.path, '_rel'), join(getcwd(), '_rel'))
            return True
        except subprocess.CalledProcessError as e:
            error(str(e))
            return False
        except RuntimeError as e:
            error(str(e))
            return False
        finally:
            if erts_tarball is not None:
                remove(erts_tarball)

    def export(self) -> dict:
        return {'release': self.params}

    def __download_erts(self, coon_cache, path):
        if coon_cache is None:
            raise RuntimeError('No official coon cache in config')
        downloaded_path = coon_cache.fetch_erts(self.params['erlang'])
        untar(downloaded_path, path)
        return downloaded_path
