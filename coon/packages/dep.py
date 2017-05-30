class Dep:
    def __init__(self, url: str, branch: str or None, tag=None):
        self._url = url
        self._tag = tag
        self._branch = branch

    @property
    def url(self) -> str:
        return self._url

    @property
    def tag(self) -> str:
        return self._tag

    @property
    def branch(self) -> str:
        return self._branch
