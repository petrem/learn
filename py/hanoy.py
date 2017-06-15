def print_status(towers):
    n = len(towers[0])
    digits = len(str(n))
    for i in range(n-1, -1, -1):
        for t in towers:
            print ('{0:^%d}' % (digits+2*n)).format('{1}{0}{1}'.format(t[i],'-'*(t[i]))),
        print



def generate_initial(n):
    towers = [[],[0]*n,[0]*n]
    for i in range(n, 0, -1):
        towers[0].append(i)
    return towers


if __name__ == "__main__":
    initial = generate_initial(4)
    print_status(initial)
