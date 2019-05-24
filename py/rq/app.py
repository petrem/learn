import time

from rq import Queue
from redis import Redis

from tasks import count_words_at_url, content_at_url


if __name__ == "__main__":
    # Tell RQ what Redis connection to use
    redis_conn = Redis()
    q = Queue(connection=redis_conn)  # no args implies the default queue

    # Delay execution of count_words_at_url('http://nvie.com')
    jobs = [
        q.enqueue(count_words_at_url, 'http://nvie.com'),
        q.enqueue(content_at_url, 'http://nvie.com')
    ]

    # Now, wait a while, until the worker is finished
    while any(not job.is_finished for job in jobs):
        time.sleep(0.5)

    for job in jobs:
        job.refresh()
        print(job.result, job.meta)
