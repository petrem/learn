def classify(number):
    if number <= 0:
        raise ValueError("Cannot classify negative or zero number")
    aliquot_sum = sum(x for x in range(1, number - 1) if number % x == 0)
    if aliquot_sum == number:
        return "perfect"
    if aliquot_sum < number:
        return "deficient"
    return "abundant"
