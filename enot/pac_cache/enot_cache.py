from os.path import join

from enot.pac_cache.cache import CacheType
from enot.pac_cache.remote_cache import RemoteCache
from enot.packages.package import Package
from enot.utils.http_utils import download_file, post_redirect, get_redirect
from enot.utils.logger import warning, info


class EnotCache(RemoteCache):
    def __init__(self, temp_dir: str, default_erlang: str, conf: dict):
        name = conf['name']
        cache_url = conf['url']
        super().__init__(name, temp_dir, cache_url, default_erlang, CacheType.ENOT)

    def get_versions(self, fullname: str) -> list:
        versions = self._get_versions(fullname)
        return [pv['ref'] for pv in versions]

    def get_erl_versions(self, fullname: str, version: str) -> list:
        versions = self._get_versions(fullname, version)
        return [pv['erl_version'] for pv in versions]

    def fetch_version(self, fullname: str, version: str) -> Package or None:
        [name] = fullname.split('/')[-1:]
        write_path = self.__download_package(name, fullname, version)
        return Package.from_package(write_path)

    def add_package(self, package: Package, rewrite=True) -> bool:
        raise RuntimeError('Not implemented')

    def fetch_package(self, package: Package):
        write_path = self.__download_package(package.name, package.fullname, package.git_vsn)
        package.update_from_package(write_path)

    def fetch_erts(self, erlang_vsn: str) -> str:
        info('fetch erts for ' + erlang_vsn)
        return self.__download_release(erlang_vsn)

    def _get_versions(self, fullname, ref=None) -> [dict]:
        url = join(self.path, 'versions')
        data = {'full_name': fullname}
        if ref is not None:
            data['versions'] = {'ref': ref}
        r = post_redirect(url, data, {'Content-type': 'application/json'})
        json = r.json()
        if json['result'] is not True:
            warning('Error accessing ' + url + ': ' + json['response'])
            return []
        return json['response']

    def __download_package(self, name: str, fullname: str, version: str) -> str:
        url = join(self.path, 'get')
        write_path = join(self.temp_dir, name + '.ep')
        r = post_redirect(url,
                          {'full_name': fullname,
                           'versions': [{'ref': version, 'erl_version': self.erlang_version}]},
                          {'Content-type': 'application/json'})
        download_file(r, write_path, b'No such build', 'Package ' + fullname + ':' + version + ' not found')
        return write_path

    def __download_release(self, version: str) -> str:
        url = join(self.path, 'download_erts/' + version)
        write_path = join(self.temp_dir, version + '.tar')
        r = get_redirect(url)
        download_file(r, write_path, b'No such erlang', 'No such erlang version: ' + version)
        return write_path
