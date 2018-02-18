import fileinput
import shutil
import subprocess
import sys
from os import remove, listdir
from os.path import join

from enot.action.action import Action
from enot.compiler.relx import RelxCompiler
from enot.utils.file_utils import untar, remove_dir, ensure_executable
from enot.utils.logger import error


class Release(Action):
    def __init__(self, params: dict):
        super().__init__()
        self._params = params

    @property
    def params(self) -> dict:
        return self._params

    def run(self, path, package=None, system_config=None, erlang_vsn: str = None) -> bool:
        enot_cache = system_config.cache.official_cache
        rel_dir = self.params['rel_dir']
        erts_tarball = None
        try:
            erts_tarball, erts_path = Release.__download_erts(enot_cache, package.path, erlang_vsn)
            compiler = RelxCompiler(package)
            compiler.ensure_tool(system_config.cache.local_cache)
            if not compiler.compile(params=['-i', package.path], erts=erts_path):
                return False
            local = join(rel_dir, '_rel')
            remove_dir(local)
            shutil.copytree(join(package.path, '_rel'), local)
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

    @staticmethod
    def __download_erts(enot_cache, path, erlang_vsn):
        if enot_cache is None:
            raise RuntimeError('No official enot cache in config')
        downloaded_path = enot_cache.fetch_erts(erlang_vsn)
        untar(downloaded_path, path)
        erts_path = Release.__determine_erts(path)
        Release.__change_erts_root(erts_path, path)
        return downloaded_path, erts_path

    @staticmethod
    def __determine_erts(path):
        erts = [f for f in listdir(path) if f.startswith("erts-")]
        if not erts:
            raise RuntimeError('Erts not found in ' + path)
        erts_bin = join(path, erts[0], 'bin')
        for f in listdir(erts_bin):  # make all files executable so relx can run
            ensure_executable(join(erts_bin, f))
        return erts_bin

    @staticmethod
    def __change_erts_root(erts, path):
        for i, line in enumerate(fileinput.input(join(erts, 'erl'), inplace=True)):
            if line.startswith('ROOTDIR="//erl/'):
                sys.stdout.write('ROOTDIR="' + path + '"\n')
            else:
                sys.stdout.write(line)
