from os.path import join

from coon.pac_cache.cache import Cache
from coon.packages.package import Package


class S3Cache(Cache):
    def __init__(self, temp_dir, conf):
        cache_url = conf['url']
        self._bucket = conf['bucket']
        super().__init__(conf['name'], temp_dir, cache_url)

    def exists(self, package: Package) -> bool:
        raise RuntimeError('not implemented')

    def add_package(self, package: Package, rewrite: bool):
        raise RuntimeError('not implemented')

    def fetch_package(self, package: Package) -> bool:
        raise RuntimeError('not implemented')

    def __get_package_url(self, package: Package):
        namespace = package.url.split('/')[-2]
        return join(namespace, package.name, package.vsn, self.erlang_version, package.name) + '.wp'
