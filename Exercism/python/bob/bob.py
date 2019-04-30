import string


def is_question(phrase):
    return phrase.endswith("?")


def is_shout(phrase):
    letters = list(filter(str.isalpha, phrase))
    if letters:
        print("letters", letters)
        return all(c.isupper() for c in letters)
    return False


def is_silence(phrase):
    return phrase == ""


def remove_whitespace(s):
    return s.translate(str.maketrans("", "", string.whitespace))


def hey(phrase):
    cooked_phrase = remove_whitespace(phrase)
    if is_silence(cooked_phrase):
        return "Fine. Be that way!"
    if is_shout(cooked_phrase):
        if is_question(cooked_phrase):
            return "Calm down, I know what I'm doing!"
        else:
            return "Whoa, chill out!"
    if is_question(cooked_phrase):
        return "Sure."
    return "Whatever."
