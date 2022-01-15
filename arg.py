#!/usr/bin/env python3

from sys import argv

keys = []
values = []

for arg in argv[1:]:
    i = arg.find('=') + 1

    if i:
        keys.append(arg[:i])

        t = list(map(float, arg[i:].split(':')))

        x1, dx, x2 = (t[0], 1.0, t[0]) if len(t) == 1 \
                else (t[0], 1.0, t[1]) if len(t) == 2 \
                else t

        n = int((x2 - x1) / dx) + 1

        values.append(['{:g}'.format(x1 + _ * dx) for _ in range(n)])
    else:
        keys.append(arg)
        values.append([''])

def combis(n=0):
    if n >= len(values):
        yield []
    else:
        for value in values[n]:
            for arg in combis(n + 1):
                yield [keys[n] + value] + arg

for _ in combis():
    print('\ '.join(_))
