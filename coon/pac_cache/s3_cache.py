from os.path import join

from coon.pac_cache.cache import Cache
from coon.packages.cachable import Cachable


class S3Cache(Cache):
    def __init__(self, temp_dir, conf):
        cache_url = conf['url']
        self._bucket = conf['bucket']
        super().__init__(conf['name'], temp_dir, cache_url)

    def exists(self, package: Cachable) -> bool:
        raise RuntimeError('not implemented')

    def add_package(self, package: Cachable, rewrite=True) -> bool:
        raise RuntimeError('not implemented')

    def fetch_package(self, package: Cachable) -> bool:
        raise RuntimeError('not implemented')

    def __get_package_url(self, package: Cachable):
        return join(self.get_package_path(package), package.name) + '.cp'
