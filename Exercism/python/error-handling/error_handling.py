def handle_error_by_throwing_exception():
    raise RuntimeError("Sir Fail-a-lot was here")


def handle_error_by_returning_none(input_data):
    return int(input_data) if input_data.isnumeric() else None


def handle_error_by_returning_tuple(input_data):
    try:
        return True, int(input_data)
    except ValueError:
        return False, None


def filelike_objects_are_closed_on_exception(filelike_object):
    with filelike_object as f:
        f.do_something()
