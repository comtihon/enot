from enot.tool.tool import AbstractTool


class RebarTool(AbstractTool):
    @property
    def url(self) -> str:
        return 'https://github.com/rebar/rebar/wiki/rebar'

    @property
    def name(self) -> str:
        return 'rebar'
