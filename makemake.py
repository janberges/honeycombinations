#!/Users/jan/anaconda/bin/python

import os
import re
import platform

linux = platform.system() == 'Linux'

wd = os.path.dirname(os.path.abspath(__file__))
os.chdir(wd)

names = sorted(name[:-4] for name in os.listdir('.') if '.f90' in name)

with open('makefile', 'w') as makefile:
    if linux:
        makefile.write('''# lapack:

LA = /home/jberges/la
LAPK = $(LA)/lapack-3.5.0
BLAS = $(LA)/BLAS

''')
    
    makefile.write('''# variables:

FC = gfortran
LN = $(FC)

# one may use -fcheck=all here:
FC_OPT = -O3 -std=f2003 -Wall -pedantic
''')

    if linux:
        makefile.write('LN_OPT = -L$(LAPK) -llapack -L$(BLAS) -lblas')
    else:
        makefile.write('LN_OPT = -llapack -lblas')
    
    makefile.write('''

OUT = comb

# linking:

$(OUT):''')

    for name in names:
        makefile.write(' %s.o' % name)

    makefile.write('''
	@echo linking $@
	@$(LN) -o $@ $^ $(LN_OPT)

# compiling:

%.o: %.f90
	@echo compiling $(@:.o=)
	@$(FC) $(FC_OPT) -c $<

# dependencies:
''')
	
    for name in names:
        makefile.write('\n%s.o:' % name)
        
        with open('%s.f90' % name) as file:
            for line in file:
                use = re.search('^\s*use\s+(\w+)', line, re.IGNORECASE)
                if use:
                    makefile.write(' %s.o' % use.group(1))
                
    makefile.write('''

# automatization:

var = ce=0:0.01:0.2 cX=0.01:0.01:0.2

ARGS := $(shell python arg.py $(var))

.PHONY: clean cleaner jobs $(ARGS)

clean:
	@rm -f *.o *.mod

cleaner: clean
	@rm $(OUT)

jobs: $(ARGS)

$(ARGS):
	./$(OUT) $(global) $@
''')
