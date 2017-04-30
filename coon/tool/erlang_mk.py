from coon.tool.tool import AbstractTool


class ErlangMKTool(AbstractTool):
    @property
    def url(self) -> str:
        return 'https://erlang.mk/erlang.mk'

    @property
    def name(self) -> str:
        return 'erlang.mk'
