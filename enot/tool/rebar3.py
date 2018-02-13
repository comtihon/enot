from enot.tool.tool import AbstractTool


class Rebar3Tool(AbstractTool):
    @property
    def url(self) -> str:
        return 'https://s3.amazonaws.com/rebar3/rebar3'

    @property
    def name(self) -> str:
        return 'rebar3'
