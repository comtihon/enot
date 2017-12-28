from coon.pac_cache.local_cache import LocalCache
from coon.pac_cache.cache import Cache, CacheType
from coon.pac_cache.coon_cache import CoonCache


def get_cache(cache_type: CacheType, conf: dict, tepm_dir: str) -> Cache:
    if cache_type == CacheType.LOCAL:
        return LocalCache(tepm_dir, conf)
    elif cache_type == CacheType.COON:
        return CoonCache(tepm_dir, conf)
    else:
        raise RuntimeError('Unknown cache type: ' + cache_type.value)
