FC = gfortran

flags_gfortran = -std=f2003 -pedantic -Wall -Wno-maybe-uninitialized
flags_ifort = -O0 -stand f03 -warn all
flags_ifx = ${flags_ifort}

FFLAGS = ${flags_$(FC)}

LDLIBS = -llapack -lblas

# generated by makemake90:

needless += approx.o control.o conversion.o diagonalization.o dope.o dos.o energy.o global.o hamiltonian.o main.o memory.o montecarlo.o move_one.o neighborhood.o out.o parser.o plot.o random.o timer.o transposition.o *.mod

programs = honeycombinations

.PHONY: all clean cleaner

all: $(programs)

clean:
	rm -f $(needless)

cleaner: clean
	rm -f $(programs)

$(programs):
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

honeycombinations: approx.o control.o conversion.o diagonalization.o dope.o dos.o energy.o global.o hamiltonian.o main.o memory.o montecarlo.o move_one.o neighborhood.o out.o parser.o plot.o random.o timer.o transposition.o

approx.o: global.o
control.o: diagonalization.o dope.o dos.o energy.o global.o hamiltonian.o montecarlo.o move_one.o neighborhood.o out.o parser.o plot.o random.o timer.o
conversion.o: global.o
diagonalization.o: global.o hamiltonian.o memory.o
dope.o: global.o memory.o
dos.o: diagonalization.o global.o
energy.o: approx.o diagonalization.o global.o neighborhood.o
hamiltonian.o: global.o memory.o neighborhood.o random.o
main.o: control.o
memory.o: global.o
montecarlo.o: approx.o conversion.o energy.o global.o neighborhood.o plot.o transposition.o
move_one.o: diagonalization.o global.o neighborhood.o plot.o transposition.o
neighborhood.o: global.o memory.o random.o transposition.o
out.o: global.o
parser.o: global.o
plot.o: conversion.o global.o memory.o random.o
random.o: global.o memory.o
timer.o: global.o
transposition.o: global.o

# not generated by makemake90:

cmd = ce=0:0.01:0.2 cX=0.01:0.01:0.2 bye

ARGUMENTS := $(shell python3 arg.py $(cmd))

.PHONY: jobs $(ARGUMENTS)

jobs: $(ARGUMENTS)

$(ARGUMENTS):
	./$(programs) $@
