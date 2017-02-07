from walrus.pac_cache import LocalCache
from walrus.pac_cache import RemoteCache
from walrus.pac_cache.cache import Cache


def get_cache(temp_dir: str, cache_url: str) -> Cache:
    # TODO add ability to search first local then remote.
    # TODO add ability to customise search policy (build instead of fetching from remote etc...).
    protocol = get_protocol(cache_url)
    if protocol == 'file':
        return LocalCache(temp_dir, cache_url)
    else:
        return RemoteCache(protocol, cache_url)


def get_protocol(url):
    return url.split('://')[0]
