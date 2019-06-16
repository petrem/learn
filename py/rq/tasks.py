from contextlib import closing
from collections import namedtuple
from os import fdopen
import requests
from tempfile import mkstemp

from rq import get_current_job


SpecialMe = namedtuple("SpecialMe", "as_file,path")


class ResponseWrapper():
    def __init__(self, result, as_file=False):
        self.result = result
        self.as_file = as_file


def count_words_at_url(url):
    resp = requests.get(url)
    me = get_current_job()
    me.meta['as_file'] = False
    me.save_meta()
    print(f"count task {me.id}: got response {resp.status_code} from {url}")
    print(f"my meta is now {me.meta}")
    return SpecialMe(False, len(resp.text.split()))


def content_at_url(url):
    resp = requests.get(url)
    fd, path = mkstemp()
    with closing(fdopen(fd, "wt")) as f:
        f.write(resp.text)
    me = get_current_job()
    me.meta['as_file'] = True
    me.save_meta()
    print(f"content task {me.id}: got response {resp.status_code} from {url}")
    print(f"my meta is now {me.meta}")
    return SpecialMe(True, path)
