from coon.pac_cache import ArtifactoryCache
from coon.pac_cache import LocalCache
from coon.pac_cache import S3Cache
from coon.pac_cache.cache import Cache, CacheType


def get_cache(cache_type: CacheType, conf: dict, tepm_dir: str) -> Cache:
    if cache_type == CacheType.LOCAL:
        return LocalCache(tepm_dir, conf)
    elif cache_type == CacheType.ARTIFACTORY:
        return ArtifactoryCache(tepm_dir, conf)
    elif cache_type == CacheType.S3:
        return S3Cache(tepm_dir, conf)
    else:
        raise RuntimeError('Unknown cache type: ' + cache_type.value)
