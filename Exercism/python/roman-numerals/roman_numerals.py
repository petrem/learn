def roman(number):
    return "".join(
        reversed(
            [
                rds[int(a)] for a, rds in zip(reversed(str(number)), roman_digits)
            ]
        )
    )


roman_digits = [
    ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"],
    ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"],
    ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"],
    ["M" * i for i in range(10)],
]
