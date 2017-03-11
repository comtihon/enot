from walrus.pac_cache import ArtifactoryCache
from walrus.pac_cache import LocalCache
from walrus.pac_cache.cache import Cache, CacheType


def get_cache(cache_type: str, conf:dict) -> Cache:
    if cache_type == CacheType.LOCAL:
        return LocalCache(conf['temp_dir'], conf['url'])
    elif cache_type == 'artifactory':
        return ArtifactoryCache(conf['url'])
