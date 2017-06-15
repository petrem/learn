import gmpy
import sys

def primes(limit,start=2):
    current = gmpy.mpz(start)
    while current <= limit:
       yield current
       current = current.next_prime()


if __name__ == "__main__":
    count = 0
    prime = None
    if len(sys.argv) > 1:
        start = int(sys.argv[1])
    else:
        start = 2
    if len(sys.argv) > 2:
        end = int(sys.argv[2])
    else:
        end = 1000000000000066600000000001
    for prime in primes(end, start):
        print 'prime:', prime, "digits:", prime.numdigits()
        count+=1
    print "last prime:", prime, "in total:", count


