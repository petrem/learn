#https://stackoverflow.com/questions/57945028/autovivification-of-nested-dictionary-with-lists-to-append-in-python


class AutoDict(dict):

    def __setitem__(self, keys, value):
        if not isinstance(keys, tuple):
            return dict.__setitem__(self, keys, value)

        for key in keys[:-1]:
            self = self.setdefault(key, {})
        dict.__setitem__(self, keys[-1], value)
        return self

    def __getitem__(self, key):
        try:
            return dict.__getitem__(self, key)
        except KeyError:
            return self.setdefault(key, [])
