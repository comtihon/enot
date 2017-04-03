from walrus.pac_cache import Cache
from walrus.packages.package import Package


class ArtifactoryCache(Cache):
    def __init__(self, temp_dir, conf):
        cache_url = conf['url']
        super().__init__(conf['name'], temp_dir, cache_url)

    def exists(self, package: Package):
        pass

    def add_package(self, package: Package, rewrite: bool):
        pass

    def fetch_package(self, package: Package) -> bool:
        pass
