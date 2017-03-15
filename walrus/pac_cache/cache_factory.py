from walrus.pac_cache import ArtifactoryCache
from walrus.pac_cache import LocalCache
from walrus.pac_cache import S3Cache
from walrus.pac_cache.cache import Cache, CacheType


def get_cache(cache_type: str, conf: dict, tepm_dir: str) -> Cache:
    if cache_type == CacheType.LOCAL.value:
        return LocalCache(tepm_dir, conf)
    elif cache_type == CacheType.ARTIFACTORY.value:
        return ArtifactoryCache(tepm_dir, conf)
    elif cache_type == CacheType.S3.value:
        return S3Cache(tepm_dir, conf)
    else:
        raise RuntimeError('Unknown cache type: ' + cache_type)