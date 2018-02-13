from enot.tool.tool import AbstractTool


class ErlangMKTool(AbstractTool):
    @property
    def url(self) -> str:
        return 'https://erlang.mk/erlang.mk'

    @property
    def name(self) -> str:
        return 'erlang.mk'

    @property
    def local_executable(self) -> str:
        return 'make'  # still callinkg make. No matter if erlang.mk is linked.
