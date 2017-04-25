import subprocess
from subprocess import PIPE
from urllib.error import HTTPError
from urllib.request import Request, urlopen

from coon.compiler import AbstractCompiler
from coon.utils.file_utils import get_cmd, write_file


class ErlangMKCompiler(AbstractCompiler):
    def compile(self):
        # TODO ensure Makefile and ensure it includes erlang.mk
        p = subprocess.Popen('make', stdout=PIPE, stderr=PIPE, cwd=self.root_path)
        if p.wait() != 0:
            print(self.project_name + ' compilation failed: ')
            print(p.stderr.read().decode('utf8'))
            print(p.stdout.read().decode('utf8'))
            return False
        else:
            return True

    # if erlang.mk is not in dir - try to download it from off cite.
    @staticmethod
    def ensure(path):
        tool_path = get_cmd(path, 'erlang.mk')
        if tool_path is None:
            req = Request('https://erlang.mk/erlang.mk', headers={'User-Agent': 'Mozilla/5.0'})
            try:
                content = urlopen(req).read()
            except HTTPError as e:
                print('Could not fetch erlang.mk: ' + str(e.reason))
                raise RuntimeError('Could not obtain erlang.mk')
            write_file(path, content)
            tool_path = './erlang.mk'
        return tool_path
