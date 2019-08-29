import sys


def fun1():
    print("line1")


def fun2():
    print("line2")


def fun3(arg):
    if arg == "bing":
        print("line3")


fun1()
fun2()
fun3(sys.argv[1])
