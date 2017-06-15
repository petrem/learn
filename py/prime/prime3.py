#!/usr/bin/env python3
import csv
import io
import gmpy
import os
import numpy
import sys


PRIMES = 'primes.csv'


def primes(limit, start=2):
    current = gmpy.mpz(start)
    while current <= limit:
        yield current
        current = current.next_prime()


def last_line(primes_file):
    BLOCK_SIZE = 4096

    def get_block(block_idx, block_ptr):
        if block_ptr > BLOCK_SIZE:
            f.seek(block_idx * BLOCK_SIZE, 2)
            block = f.read(BLOCK_SIZE)
        else:
            f.seek(0, 0)
            block = f.read(block_ptr)
        return block

    blocks = []
    block_index = -1
    count = 1

    with io.open(primes_file, 'rb') as f:
        end_pos = f.seek(0, 2)
        if end_pos == 0:
            return None
        f.seek(-1, 2)
        if f.read(1) == b'\n':
            count = 2
        looking_for = count
        while looking_for > 0 and end_pos > 0:
            block = get_block(block_index, end_pos)
            blocks.append(block)
            newlines = block.count(b'\n')
            looking_for -= newlines
            end_pos -= BLOCK_SIZE
            block_index -= 1
        buffer = b''.join(reversed(blocks))
        start_pos = buffer.rfind(b'\n', 0, len(buffer) - (count - 1))
        return buffer[start_pos+1:]


def prime_from_line(line):
    r = csv.reader([line])
    row = next(r)
    return int(row[0])


if __name__ == "__main__":
    count = 0
    line = last_line(PRIMES)
    if line:
        start = prime_from_line(str(line, encoding='utf8'))
    else:
        start = 2
    end = 1000000000000066600000000001
    with io.open(PRIMES, 'at') as f:
        w = csv.writer(f)
        try:
            prev_prime = 2
            for prime in primes(end, start):
                diff = prime - prev_prime
                w.writerow([prime, prev_prime, diff])
                print('prime: {}, digits: {}'.format(prime, prime.numdigits()))
                count += 1
                prev_prime = prime
        except:
            raise
            print("last prime: {}, in total: {}".format(prime, count))
            sys.exit(0)

