#!/usr/local/bin/python3

import contextlib
import os


def get_user():
    ruid = os.getuid()
    if ruid != 0:
        return ruid
    try:
        sudo_uid = os.environ['SUDO_UID']
        return int(sudo_uid)
    except (KeyError, ValueError):
        raise RuntimeError("Could not determine unprivileged user id")


@contextlib.contextmanager
def temporary_dropped_privs(uid=None):
    saved_euid = os.geteuid()
    if uid is None:
        uid = get_user()
    os.seteuid(uid)
    yield
    os.seteuid(saved_euid)


def drop_privs(uid=None):
    if uid is None:
        uid = get_user()
    os.setuid(uid)


if __name__ == "__main__":
    def print_ids():
        print("ruid", os.getuid())
        print("euid", os.geteuid())

    print("Start")
    print_ids()
    with temporary_dropped_privs():
        print("Temp dropped")
        print_ids()
    print("And back")
    print_ids()
    drop_privs()
    print("Dropped")
    print_ids()
