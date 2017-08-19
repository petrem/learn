#!/usr/bin/env python3


class GenderMeta(type):

    def __new__(cls, *args, **kwargs):
        # print("Meta.__new__({},{}".format(args, kwargs))
        klass = type.__new__(cls, *args, **kwargs)

        males = [1, "1", "m", "M", "male", "Male", "MALE"]
        females = [2, "2", "f", "F", "female", "Female", "FEMALE"]
        unknowns = [0, None]
        value_map = {k: 1 for k in males}
        value_map.update({k: 2 for k in females})
        value_map.update({k: 0 for k in unknowns})
        setattr(klass, "VALUE_MAP", value_map)

        return klass

    # def __call__(cls, *args, **kwargs):
    #     # print("Meta.__call__({},{}".format(args, kwargs))
    #     return super().__call__(*args, **kwargs)

    @property
    def Male(cls):
        return cls(1)

    @property
    def Female(cls):
        return cls(2)

    @property
    def Unknown(cls):
        return cls(0)


class Gender(int, metaclass=GenderMeta):

    _instances = [None, None, None]

    @classmethod
    def _map(cls, value):
        return cls.VALUE_MAP.get(value)

    def __new__(cls, value):
        # print("Class.__new__({},{})".format(cls, value))
        m = cls._map(value)
        if m is None:
            raise ValueError(
                "Cannot convert {} of type {} to a gender".format(
                    value, type(value)))
        if cls._instances[m] is None:
            cls._instances[m] = super().__new__(cls, m)
        return cls._instances[m]

    def __repr__(self):
        return {
            1: "Male",
            2: "Female"
        }.get(self.real, "Unknown")

    def __str__(self):
        return self.__repr__()

    def __eq__(self, other):
        if isinstance(other, Gender):
            return self is other
        else:
            return self.real == self._map(other)


x = Gender(1)
print(Gender.Male, type(Gender.Male))
print(x, type(x))
print("x equals Male", x == Gender.Male)
print("x equals Female", x == Gender.Female)
print("2 equalas Female", 2 == Gender.Female)
print("3 equals Male", 3 == Gender.Male)
print("Gender(M)", Gender('M'))
print(Gender('m') == Gender.Male)
print(Gender('m') is Gender.Male)
