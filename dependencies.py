#!/usr/bin/env python3

import os
import re

wd = os.path.dirname(os.path.abspath(__file__))
os.chdir(wd)

names = sorted(name[:-4] for name in os.listdir('.') if '.f90' in name)

sub = None

for name in names:
    print(name.upper())

    with open('%s.f90' % name) as file:
        for line in file:
            match = re.search(r'(\bend\s+)?\bsubroutine\s+(\w+)', line, re.I)
            if match and not match.group(1):
                sub = match.group(2)
                continue

            match = re.search(r'if\s*\(todo%(\w+)', line, re.I)
            if match:
                print('dependency: ' + sub + ' < ' + match.group(1))
                continue

            match = re.search(r'todo%(\w+)\s*=\s*\.(true|false)\.', line, re.I)
            if match:
                print(('side effect: ' if match.group(2) == 'true'
                    else 'job done: ') + sub + ' > ' + match.group(1))
                continue
