class Quack:
    DEFAULT = "Quack! Quack!"

    def __get__(self, obj, obj_type=None):
        print(f">>> Quacks like {obj} of type {obj_type} <<<")
        try:
            return obj.QUACK
        except AttributeError:
            return Quack.DEFAULT


class Look:
    def __get__(self, obj, obj_type):
        print(f">>> Looks like {obj} <<<")
        return lambda: "a duck!"


class Duck:
    quack = Quack()
    look = Look()


class Scaup(Duck):
    """I'm a kind of a duck"""
    QUACK = "Scaup! Scaup!"


# looking up on the class
print(f"All ducks quack: {Duck.quack}\n")

# looking up on an object
a_duck = Duck()
print(f"A duck quacks like {a_duck.quack}\n")

a_scaup = Scaup()
print(f"A scaup quacks like {a_scaup.quack}\n")

# descriptor returns a callable
print(f"A duck look like {a_duck.look} ... ooops\n")
print(f"Again, a duck look() like {a_duck.look()}\n")

print()

print(Look().__get__(a_duck, Duck)())
