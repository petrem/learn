import os
from pathlib import Path


def which(command):
    """Search PATH directories for a file named command, returning the first match."""
    try:
        path = next(
            p for p in map(Path, os.environ["PATH"].split(":"))
            if p.is_dir() and any(f.name == "bash" for f in p.iterdir()))
        return path / command
    except StopIteration:
        return None
