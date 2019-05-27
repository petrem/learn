import contextlib
from functools import partial
import io
import sys
import traceback


@contextlib.contextmanager
def capture_task_response(response={}):
    """Capture stdout and stderr and form response dictionary."""
    string_buffer = io.StringIO()
    try:
        orig_stdout, orig_stderr = sys.stdout, sys.stderr
        sys.stdout = sys.stderr = string_buffer
        yield response
        response["status"] = "ok"
    except Exception:
        response["status"] = "error"
        traceback.print_exc()
    finally:
        response["output"] = string_buffer.getvalue()
        sys.stdout, sys.stderr = orig_stdout, orig_stderr
        string_buffer.close()


@contextlib.contextmanager
def capture_task_response2(response={}):
    """Capture stdout and stderr and form response dictionary."""
    string_buffer = io.StringIO()
    with contextlib.redirect_stdout(string_buffer), contextlib.redirect_stderr(
        string_buffer
    ):
        try:
            yield response
            response["status"] = "ok"
        except Exception:
            response["status"] = "error"
            traceback.print_exc()
        finally:
            response["output"] = string_buffer.getvalue()
            string_buffer.close()


@contextlib.contextmanager
def debugctxt():
    try:
        print("debugctxt: enter", file=sys.stderr)
        yield
        print("debugctxt: exit no error", file=sys.stderr)
    except Exception as e:
        print(f"debugctxt: exit with error {e}", file=sys.stderr)
    finally:
        print("debugctxt: finally", file=sys.stderr)


class TaskResponse(contextlib.ExitStack):
    def __init__(self):
        super().__init__()
        self._response = None
        self._log_buffer = io.StringIO()
        self._saved_log = []
        self._output = None

    def __enter__(self):
        super().__enter__()
        self.enter_context(contextlib.redirect_stdout(self._log_buffer))
        #self.enter_context(contextlib.redirect_stderr(self._log_buffer))
        #self.enter_context(debugctxt())
        #print("debug: __enter__", file=sys.stderr)
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        #print("debug: __exit__", file=sys.stderr)
        response = {}
        # copy saved log chuncks
        output = list(self._saved_log)
        # add the latest log chunck
        output.append(self._log_buffer.getvalue())

        if exc_type is None:
            response["status"] = ("ok")
        else:
            response["status"] = "error"
            output.append(
                "".join(
                    traceback.format_exception(exc_type, exc_val, exc_tb)
                )
            )
        super().__exit__(exc_type, exc_val, exc_tb)
        response["output"] = "".join(output)
        self._response = response
        return True

    @property
    def log(self):
        return self._log_buffer.getvalue()

    def set_status(self, status):
        self._response["status"] = status

    def reset_log_buffer(self):
        current = self._log_buffer.getvalue()
        #print(f"[current: {current}]", file=sys.stderr)
        self._saved_log.append(current)
        self._log_buffer.truncate(0)
        self._log_buffer.seek(0, 0)
        #print(f"[saved: {self._saved_log}]", file=sys.stderr)
        #print(f"[truncated: {self._log_buffer.getvalue()}]", file=sys.stderr)

    @property
    def response(self):
        if self._response is None:
            raise RuntimeError("response not ready")
        return self._response

    def __str__(self):
        return str(self._response)


def _success():
    print("i am success!")
    return 1


def _error():
    print("I am error!")
    raise RuntimeError("boo hoo")
    return 2


def success():
    with TaskResponse() as r:
        _success()
    return r


def error():
    with TaskResponse() as r:
        _error()
    return r


def _iterating(fn):
    for i in range(5):
        yield f"{i}: {fn()}"


def iterating(fn):
    with TaskResponse() as r:
        for i in _iterating(fn):
            print(f"Iteration result: `{i}` and buffer: `{r.log}`")
            r.reset_log_buffer()
    return r


def test():
    for fn in (
        success,
        error,
        partial(iterating, _success),
        partial(iterating, _error)
    ):
        r = fn()
        print(f"{fn}: {r.response}", file=sys.stderr)


if __name__ == "__main__":
    test()
    # with TaskResponse() as r:
    #     print("In context")
    # print("after with", file=sys.stderr)
    # print(r, file=sys.stderr)
    # print("done")
    # print("final buffer value:" + repr(r._string_buffer.getvalue()), file=sys.stderr)
