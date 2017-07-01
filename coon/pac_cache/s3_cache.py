from os.path import join

from coon.packages.package import Package
from coon.pac_cache.remote_cache import RemoteCache


class S3Cache(RemoteCache):  # TODO implement me
    def __init__(self, temp_dir, conf):
        cache_url = conf['url']
        self._bucket = conf['bucket']
        super().__init__(conf['name'], temp_dir, cache_url)

    def exists(self, package: Package) -> bool:   # TODO support branches
        raise RuntimeError('not implemented')

    def add_package(self, package: Package, rewrite=True) -> bool:
        raise RuntimeError('not implemented')

    def fetch_package(self, package: Package) -> bool:
        raise RuntimeError('not implemented')

    def get_versions(self, fullname: str) -> list:
        raise RuntimeError('not implemented')

    def fetch_version(self, fullname: str, version: str) -> Package or None:
        raise RuntimeError('not implemented')

    def __get_package_url(self, package: Package):
        return join(self.get_package_path(package), package.name) + '.cp'
