from os.path import join

import boto
import boto.s3.connection
from boto.s3.connection import S3Connection
from boto.s3.key import Key

from walrus.pac_cache import Cache
from walrus.packages import Package


class S3Cache(Cache):
    def __init__(self, temp_dir, conf):
        cache_url = conf['url']
        self._bucket = conf['bucket']
        super().__init__(conf['name'], temp_dir, cache_url)
        self._connection = boto.connect_s3(
            aws_access_key_id=conf['access_key'],
            aws_secret_access_key=conf['secret_key'],
            host=conf['host'],
            is_secure=conf['sse'],
            calling_format=boto.s3.connection.OrdinaryCallingFormat(),
        )

    @property
    def bucket(self) -> str:
        return self._bucket

    @property
    def connection(self) -> S3Connection:
        return self._connection

    def exists(self, package: Package) -> bool:
        bucket = self.connection.get_bucket(self.bucket)
        return bucket.get_key(self.__get_package_url(package))

    def add_package(self, package: Package, rewrite: bool):  # TODO call this function. Get package from current dir or overwrite by param
        bucket = self.connection.get_bucket(self.bucket)
        if not rewrite and bucket.get_key(self.__get_package_url(package)) is not None:
            return
        key = Key(bucket)
        key.key = self.__get_package_url(package)
        key.set_contents_from_filename(package.name)

    def fetch_package(self, package: Package) -> bool:
        bucket = self.connection.get_bucket(self.bucket)
        key = Key(bucket)
        key.key = self.__get_package_url(package)
        key.get_contents_to_filename(join(self.temp_dir, package.name) + '.wp')
        return True

    def __get_package_url(self, package: Package):
        namespace = package.url.split('/')[-2]
        return join(namespace, package.name, package.vsn, self.erlang_version, package.name) + '.wp'
