from coon.packages.cachable import Cachable


class Dep(Cachable):
    def __init__(self, name: str, vsn: str, url: str) -> None:
        self._name = name
        self._vsn = vsn
        self._url = url

    @property
    def name(self):
        return self._name

    @property
    def vsn(self):
        return self._vsn

    @property
    def url(self):
        return self._url
