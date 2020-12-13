import os
from pathlib import Path


BASE_PATH = Path(__file__).parent.absolute()


class Config:
    _ENV_PREFIX = "MICROBLOG_"
    SECRET_KEY = "you'll probably guess it!"
    SQLALCHEMY_DATABASE_URI = f"sqlite:///{BASE_PATH / 'app.db'}"
    SQLALCHEMY_TRACK_MODIFICATIONS = False

    def __init__(self):
        envvar: str
        envval: str
        for envvar, envval in os.environ.items():
            confkey = envvar.replace(self._ENV_PREFIX, "", 1)
            if envvar.startswith(self._ENV_PREFIX) and hasattr(self, confkey):
                setattr(self, confkey, envval)
