from os.path import join

from git import Repo

from walrus.packages import Dep
from walrus.utils.file_utils import remove_dir
from walrus.global_properties import WalrusGlobalProperties


class WalrusPackage:
    name = ""
    package_version = ""
    url = ""
    namespace = ""
    erlang_version = ""

    def __init__(self, walrus_dep: Dep, config: WalrusGlobalProperties):
        self.name = walrus_dep.name
        self.erlang_version = config.erlang_version
        self.package_version = walrus_dep.vsn
        self.url = walrus_dep.url
        self.namespace = walrus_dep.url.split('/')[-2]

    def get_package_path(self):
        return join(self.name, self.namespace, self.package_version, self.erlang_version)

    def fetch_package(self, temp_dir: str) -> str:
        temp_path = join(temp_dir, self.name)
        print('-----> ' + temp_path)
        remove_dir(temp_dir)
        repo = Repo.clone_from(self.url, temp_path)
        assert not repo.bare
        tag = repo.create_head(self.package_version)
        repo.head.reference = tag
        repo.head.reset(index=True, working_tree=True)
        return temp_path
