import string


alphabet = set(string.ascii_lowercase)


def is_pangram(sentence):
    letters = {letter.lower() for letter in sentence if letter.lower() in alphabet}
    return letters == alphabet
