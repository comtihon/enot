from os.path import join

from artifactory import ArtifactoryPath

from coon.pac_cache.remote_cache import RemoteCache
from coon.packages.package import Package
from coon.utils.logger import info, warning


class ArtifactoryCache(RemoteCache):
    def __init__(self, temp_dir, conf: dict):
        cache_url = conf['url']
        name = conf['name']
        self._username = conf.get('username', None)
        if not self._username:
            raise SyntaxError('username is required in ' + name)
        self._password = conf.get('password', None)
        self._api_key = conf.get('api_key', None)
        super().__init__(name, temp_dir, cache_url)

    @property
    def username(self) -> str:
        return self._username

    @property
    def password(self) -> str or None:
        if self._password:
            return self._password
        else:
            return self._api_key

    def exists(self, package: Package):
        path = self.maybe_auth_access(join(self.path, self.get_package_path(package)))
        return path.exists()

    def get_user_package_path(self, package: Package) -> str:
        return join(self.username, package.name, package.git_vsn, self.erlang_version)

    def add_package(self, package: Package, rewrite=True) -> bool:
        if not rewrite and self.exists(package):
            return True
        info('Add ' + package.name + ' to ' + self.name)
        path = self.maybe_auth_access(join(self.path, self.get_user_package_path(package)))
        if not path.exists():
            path.mkdir()  # exist_ok doesn't work there on python3.2-3.5
        coon_package = join(package.path, package.name + '.cp')
        path.deploy_file(coon_package)
        return True

    def fetch_package(self, dep: Package):
        path = self.maybe_auth_access(join(self.path, self.get_package_path(dep), dep.name + '.cp'))
        write_path = join(self.temp_dir, dep.name + '.cp')
        with path.open() as fd:
            with open(write_path, 'wb') as out:
                out.write(fd.read())
        dep.update_from_package(write_path)

    def fetch_version(self, fullname: str, version: str) -> Package or None:
        [name] = fullname.split('/')[-1:]
        path = self.maybe_auth_access(join(self.path, fullname, version, self.erlang_version, name + '.cp'))
        if not path.exists():
            warning('No built package in ' + self.name + ' for your Erlang '
                    + self.erlang_version + ', available are: ' + str(self.get_erl_versions(fullname, version)))
            return None

        write_path = join(self.temp_dir, name + '.cp')
        with path.open() as fd:
            with open(write_path, 'wb') as out:
                out.write(fd.read())
        return Package.from_package(write_path)

    def get_versions(self, fullname: str) -> list:
        path = self.maybe_auth_access(join(self.path, fullname))
        if not path.exists():
            return []
        return ArtifactoryCache.listdir(path)

    def get_erl_versions(self, fullname: str, version: str):
        path = self.maybe_auth_access(join(self.path, fullname, version))
        if not path.exists():
            return []
        return ArtifactoryCache.listdir(path)

    def maybe_auth_access(self, path):
        if self.password:
            return ArtifactoryPath(path, auth=(self.username, self.password))
        else:
            return ArtifactoryPath(path)

    @staticmethod
    def listdir(path: ArtifactoryPath) -> list:
        listing = []
        for d in path:
            listing.append(d.name)
        return listing
