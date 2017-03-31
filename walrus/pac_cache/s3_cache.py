import boto
import boto.s3.connection
from boto.s3.connection import S3Connection

from walrus.pac_cache import Cache
from walrus.packages import Package


class S3Cache(Cache):
    def __init__(self, temp_dir, conf):
        cache_url = conf['url']
        super().__init__(temp_dir, cache_url)
        self._connection = boto.connect_s3(
            aws_access_key_id=conf['access_key'],
            aws_secret_access_key=conf['secret_key'],
            host=conf['host'],
            is_secure=conf['sse'],
            calling_format=boto.s3.connection.OrdinaryCallingFormat(),
        )

    @property
    def connection(self) -> S3Connection:
        return self._connection

    def exists(self, package: Package):
        pass

    def add_package(self, package: Package, rewrite: bool):
        pass

    def fetch_package(self, package: Package) -> bool:
        pass

    def get_package(self, package: Package):
        pass
