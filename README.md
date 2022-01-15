# honeycombinations

Simulate adsorbate structures on graphene via tight binding and Monte Carlo.

## Installation

A Fortran compiler, BLAS, and LAPACK are required.

    make FC=gfortran FFLAGS=-O3

## Documentation

The program `honeycombinations` will prompt you for input.

### Commands

The following commands are supported:

* `.`, `show`
* `clear`
* `vary`
* `move`
* `H`, `Hamiltonian`, `h`, `hamiltonian`
* `E`, `energy`
* `P`
* `dos`, `DOS`
* `seed`, `sow`
* `go`, `go_sighted`
* `go_blind`
* `mix`
* `mix2`
* `0`
* `move1`
* `bye`

### Input parameters

The following parameters can be set via `parameter=value` (without spaces).

* `l`, `size`
* `cX`, `coverage`
* `ce`, `doping`
* `nX`, `adatoms`
* `ne`, `electrons`
* `eC`
* `t`
* `eX`
* `V`
* `p`, `penalties`
* `kT`
* `R`
* `n`
* `show`
* `rrr`
* `time`
* `color`
* `jobz`
* `d`
* `C`
* `X`
* `filename`
* `new_order`, `order`

### Output results

The following results can be saved via `result>filename` (without spaces).

* `psi`
* `ls`, `config`
* `E`, `energies`
* `P`, `chances`
* `table`
* `matches`
