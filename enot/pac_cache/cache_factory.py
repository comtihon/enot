from enot.pac_cache.local_cache import LocalCache
from enot.pac_cache.cache import Cache, CacheType
from enot.pac_cache.enot_cache import EnotCache


def get_cache(cache_type: CacheType, conf: dict, tepm_dir: str, default_erlang: str) -> Cache:
    if cache_type == CacheType.LOCAL:
        return LocalCache(tepm_dir, default_erlang, conf)
    elif cache_type == CacheType.ENOT:
        return EnotCache(tepm_dir, default_erlang, conf)
    else:
        raise RuntimeError('Unknown cache type: ' + cache_type.value)
