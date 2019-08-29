from contextlib import closing
from collections import namedtuple
from os import fdopen, environ
import requests
from tempfile import mkstemp
import signal
import sys

from rq import get_current_job


SpecialMe = namedtuple("SpecialMe", "as_file,path,message")


class ResponseWrapper():
    def __init__(self, result, as_file=False):
        self.result = result
        self.as_file = as_file


def _handler(signum, frame):
    sys.exit()


def count_words_at_url(url):
    rcfile = environ.get('COVERAGE_PROCESS_START')
    message = f"rc: {rcfile}"
    signal.signal(signal.SIGTERM, _handler)
    try:
        resp = requests.get(url)
    except:
        return SpecialMe(False, 0, message)
    me = get_current_job()
    me.meta['as_file'] = False
    me.save_meta()
    print(f"count task {me.id}: got response {resp.status_code} from {url}")
    print(f"my meta is now {me.meta}")
    return SpecialMe(False, len(resp.text.split()), message)


def content_at_url(url):
    try:
        resp = requests.get(url)
    except:
        return SpecialMe(False, 0, "foo")
    fd, path = mkstemp()
    with closing(fdopen(fd, "wt")) as f:
        f.write(resp.text)
    me = get_current_job()
    me.meta['as_file'] = True
    me.save_meta()
    print(f"content task {me.id}: got response {resp.status_code} from {url}")
    print(f"my meta is now {me.meta}")
    return SpecialMe(True, path, "bar")
