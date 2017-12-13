from os.path import join

import requests

from coon.pac_cache.remote_cache import RemoteCache
from coon.packages.package import Package
from coon.utils.logger import warning, debug


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
        url = join(self.path, 'buildAsync')
        if package.url is None:
            warning('No url for package')
            return False
        body = {'full_name': package.fullname,
                'clone_url': package.url,
                'versions': [{'erl_version': self.erlang_version, 'ref': package.git_vsn}]
                }
        r = requests.post(url, data=body)
        debug('Issue build order: ' + str(body))
        return r.json()['result']

    def fetch_package(self, package: Package):
        write_path = self.__download_package(package.name, package.fullname, package.git_vsn)
        package.update_from_package(write_path)

    def exists(self, package: Package) -> bool:
        url = join(self.path, 'builds')
        r = requests.post(url, data={'full_name': package.fullname,
                                     'versions': [{'ref': package.git_vsn, 'erl_version': self.erlang_version}]})
        json = r.json()
        if json['result'] is not True:
            warning('Error accessing ' + url + ': ' + json['response'])
            return False
        return json['response'] is not []

    def _get_versions(self, fullname, ref=None) -> [dict]:
        url = join(self.path, 'versions')
        data = {'full_name': fullname}
        if ref is not None:
            data['versions'] = {'ref': ref}
        r = requests.post(url, data=data)
        json = r.json()
        if json['result'] is not True:
            warning('Error accessing ' + url + ': ' + json['response'])
            return []
        return json['response']

    def __download_package(self, name: str, fullname: str, version: str):
        url = join(self.path, 'get')
        write_path = join(self.temp_dir, name + '.cp')
        r = requests.post(url, data={'full_name': fullname,
                                     'versions': [{'ref': version, 'erl_version': self.erlang_version}]})
        with open(write_path, 'wb') as fd:
            for chunk in r.iter_content(chunk_size=128):
                fd.write(chunk)
        return write_path
