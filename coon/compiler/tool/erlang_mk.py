from os.path import join
from urllib.error import HTTPError
from urllib.request import Request, urlopen

from coon.compiler import Compiler
from coon.compiler.tool import AbstractTool
from coon.utils.file_utils import write_file


class ErlangMKTool(AbstractTool):
    @property
    def url(self) -> str:
        return 'https://erlang.mk/erlang.mk'

    @property
    def version(self) -> str:
        return ''

    @property
    def name(self) -> str:
        return 'erlang.mk'

    @property
    def compiler(self):
        return Compiler.BOOTSTRAP

    def build(self, src_path: str) -> str or None:
        req = Request(self.url, headers={'User-Agent': 'Mozilla/5.0'})
        try:
            content = urlopen(req).read()
        except HTTPError as e:
            print('Could not fetch erlang.mk: ' + str(e.reason))
            raise RuntimeError('Could not obtain erlang.mk')
        tool_path = join(src_path, 'erlang.mk')
        write_file(tool_path, content)
        return tool_path
