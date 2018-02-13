from requests import Response

from enot.pac_cache.remote_cache_exception import RemoteCacheException


def download_file(request: Response, write_path: str, first_bytes_check: bytes, error_str: str):
    if request.status_code != 200:
        raise RuntimeError('Error accessing remote: ' + request.text)
    first_bytes_checked = False
    with open(write_path, 'wb') as fd:
        for chunk in request.iter_content(chunk_size=128):
            if not first_bytes_checked:
                if chunk == first_bytes_check:
                    raise RemoteCacheException(error_str)
                first_bytes_checked = True
            fd.write(chunk)
