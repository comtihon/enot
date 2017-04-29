from compiler.tool import RebarTool


class Rebar3Tool(RebarTool):
    @property
    def url(self) -> str:
        return 'https://github.com/erlang/rebar3.git'

    @property
    def version(self) -> str:
        return '3.3.6'

    @property
    def name(self) -> str:
        return 'rebar3'
