from abc import ABCMeta, abstractmethod
from os.path import join
from urllib.error import HTTPError
from urllib.request import Request, urlopen

from coon.utils.file_utils import write_file
from coon.utils.logger import critical, debug


class AbstractTool(metaclass=ABCMeta):
    @property
    @abstractmethod
    def name(self) -> str:
        pass

    @property
    @abstractmethod
    def url(self) -> str:
        pass

    def ensure(self, src_path: str) -> str or None:
        debug('Fetching ' + self.name + ' ' + self.url)
        req = Request(self.url, headers={'User-Agent': 'Mozilla/5.0'})
        try:
            content = urlopen(req).read()
        except HTTPError as e:
            critical('Could not fetch ' + self.name + ': ' + str(e.reason))
            raise RuntimeError('Could not obtain ' + self.name)
        tool_path = join(src_path, self.name)
        write_file(tool_path, content, True)
        return tool_path
