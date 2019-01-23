from abc import ABCMeta, abstractmethod

# duck: drake and duck (or hen)
# mallard (a dabbling duck species)
#   scaup (a diving duck). sound: "scaup"
#   harlequin (a small sea duck). Call is distinctive mouselike squeak. Female makes coarse "ek-ek-ek." See https://www.allaboutbirds.org/guide/Harlequin_Duck/sounds
#
# domestic duck species: See https://en.wikipedia.org/wiki/List_of_duck_breeds
#   rouen
#   muscovy
#   


class Duck():
    __metaclass__ = ABCMeta

    def __str__(self):
        return "I'm a duck"

    @abstractmethod
    def quack(self):
        raise NotImplementedError


class Mallard(Duck):

    def __str__(self):
        return "I'm a mallard, a common dabbling duck"

    def quack(self):
        return "(classical) quack"


class Scaup(Mallard):
    def __str__(self):
        return "I'm a Scaup, a diving duck"

    def quack(self):
        return ["scaup", "scaup"]


def describe_quack(duck):
    def render_quack(duck):
        quack = duck.quack()
        if isinstance(quack, str):
            return quack
        elif isinstance(quack, (tuple, list)):
            return "-".join(quack)

    print(f"{duck}. I go {render_quack(duck)}.")


if __name__ == "__main__":
    ducks = [
        Mallard()
        , Scaup()
    ]
    for duck in ducks:
        describe_quack(duck)
