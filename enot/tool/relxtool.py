from enot.tool.tool import AbstractTool


class RelxTool(AbstractTool):
    def __init__(self) -> None:
        super().__init__()

    @property
    def url(self) -> str:
        return 'https://github.com/erlware/relx/releases/download/v3.22.3/relx'

    @property
    def name(self) -> str:
        return 'relx'
