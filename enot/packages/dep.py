class Dep:
    def __init__(self, url: str, branch: str or None, tag=None):
        self._url = Dep.__cut_git(url)
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

    def __eq__(self, other):
        if isinstance(other, Dep):
            return self.url == other.url and self.tag == other.tag and self.branch == other.branch
        return False

    @staticmethod
    def __cut_git(url: str):
        if url.endswith('.git'):
            return url[:-4]
        return url
