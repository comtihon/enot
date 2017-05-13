from os.path import join

from artifactory import ArtifactoryPath

from coon.pac_cache.cache import Cache
from coon.packages.package import Package


class ArtifactoryCache(Cache):
    def __init__(self, temp_dir, conf: dict):
        cache_url = conf['url']
        name = conf['name']
        self._username = conf.get('username', None)
        if not self._username:
            raise SyntaxError('username is required in ' + name)
        self._password = conf.get('password', None)
        self._api_key = conf.get('api_key', None)
        self._ssl = cache_url.startswith('https')
        if not self._password and not self._api_key:
            raise SyntaxError('password or api_key required in ' + name)
        super().__init__(name, temp_dir, cache_url)

    @property
    def username(self) -> str:
        return self._username

    @property
    def password(self) -> str:
        if self._password:
            return self._password
        else:
            return self._api_key

    @property
    def ssl(self) -> bool:
        return self._ssl

    def exists(self, package: Package):
        path = ArtifactoryPath(join(self.path, self.get_package_path(package)),
                               auth=(self.username, self.password))
        return path.exists()

    def get_package_path(self, package: Package):
        return join(self.username, package.name, package.vsn, self.erlang_version)

    def add_package(self, package: Package, rewrite=True) -> bool:
        if not rewrite and self.exists(package):
            return True
        print('Add ' + package.name + ' to ' + self.name)
        path = ArtifactoryPath(join(self.path, self.get_package_path(package)),
                               auth=(self.username, self.password))
        path.mkdir(exist_ok=True)
        coon_package = join(package.config.path, package.name + '.cp')
        path.deploy_file(coon_package)
        return True

    def fetch_package(self, package: Package) -> bool:
        path = ArtifactoryPath(join(self.path, self.get_package_path(package), package.name + '.cp'),
                               auth=(self.username, self.password))
        with path.open() as fd:
            with open(join(self.temp_dir, package.name + '.cp'), 'wb') as out:
                out.write(fd.read())
        return True
