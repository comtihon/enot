from walrus.pac_cache import LocalCache
from walrus.pac_cache import RemoteCache


def get_cache(config):
    protocol = get_protocol(config.cache_url)
    if protocol == 'file':
        return LocalCache(config)
    else:
        return RemoteCache(protocol, config)


def get_protocol(url):
    return url.split('://')[0]
