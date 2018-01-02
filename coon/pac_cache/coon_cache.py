from os.path import join

import requests

from coon.pac_cache.remote_cache import RemoteCache
from coon.pac_cache.remote_cache_exception import RemoteCacheException
from coon.packages.package import Package
from coon.utils.logger import warning


class CoonCache(RemoteCache):
    def __init__(self, temp_dir, conf: dict):
        name = conf['name']
        cache_url = conf['url']
        super().__init__(name, temp_dir, cache_url)

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

    def _get_versions(self, fullname, ref=None) -> [dict]:
        url = join(self.path, 'versions')
        data = {'full_name': fullname}
        if ref is not None:
            data['versions'] = {'ref': ref}
        r = requests.post(url, json=data, headers={'Content-type': 'application/json'})
        json = r.json()
        if json['result'] is not True:
            warning('Error accessing ' + url + ': ' + json['response'])
            return []
        return json['response']

    def __download_package(self, name: str, fullname: str, version: str):
        url = join(self.path, 'get')
        write_path = join(self.temp_dir, name + '.cp')
        r = requests.post(url,
                          json={'full_name': fullname,
                                'versions': [{'ref': version, 'erl_version': self.erlang_version}]},
                          headers={'Content-type': 'application/json'})
        first_bytes_checked = False
        with open(write_path, 'wb') as fd:
            for chunk in r.iter_content(chunk_size=128):
                if not first_bytes_checked:
                    if chunk == b'No such build':
                        raise RemoteCacheException('Package ' + fullname + ':' + version + ' not found')
                    first_bytes_checked = True
                fd.write(chunk)
        return write_path
