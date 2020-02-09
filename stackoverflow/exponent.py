def stringify_exponent(n, m):
    superscripted = "".join(_DIGITS[int(d)] for d in str(m))
    return f"{n}{superscripted}"


_DIGITS = [
    "\N{superscript zero}",
    "\N{superscript one}",
    "\N{superscript two}",
    "\N{superscript three}",
    "\N{superscript four}",
    "\N{superscript five}",
    "\N{superscript six}",
    "\N{superscript seven}",
    "\N{superscript eight}",
    "\N{superscript nine}"
]


if __name__ == "__main__":
    print(stringify_exponent(10, 2))
    print(stringify_exponent(2, 1024))
