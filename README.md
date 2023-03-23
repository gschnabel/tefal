
# TEFAL
TEFAL is a software package for the processing of the output files of the TALYS nuclear reaction code, and data from other sources, into an ENDF-6 format nuclear data library.

## Documentation and reference
The user manual for TEFAL can be found here: [TEFAL User Manual (pdf)](https://github.com/IAEA-NDS/tefal/doc/tefal.pdf).
The reference to be used for TEFAL is

A.J. Koning, D. Rochman, J.-Ch. Sublet, N. Dzysiuk, M. Fleming, and S. van der Marck, *TENDL: Complete Nuclear Data Library for innovative Nuclear Science and Technology*, Nuclear Data Sheets 155,1 (2019).

## Installation

### Prerequisites:

The following are the prerequisites for compiling TEFAL:
  - git (if the package is downloaded via Github)
  - a Fortran-1995 compliant compiler such as gcc (gfortran)
  - a successful installation of the TALYS nuclear model code

### Instructions:

To install TEFAL, you can use one of the following options:
#### 1. Using make:
```
git clone https://github.com/IAEA-NDS/tefal.git
cd tefal/source
make
```
#### 2. Using the code_build script:
```
git clone https://github.com/IAEA-NDS/tefal.git
cd tefal
code_build tefal
```

The above instructions will produce an executable in the *tefal/bin* directory. 
The compiler and its flags can be set in either the *Makefile* or in *code_build*.

## Sample cases

A successful installation can be verified by running the sample cases. For each sample case, the results are written to a subdirectory *new/*, which can then be compared with the output provided in the TEFAL package in the subdirectory *org/*. The entire sample set will take about 1.5 hours.
```
cd samples
./verify
```

TEFAL will only work after a TALYS calculation with **endf y** in the TALYS input file:
```
talys < talys,inp > talys.out
tefal < tefal.inp > tefal.out
```

## The TEFAL package

The *tefal/* directory contains the following directories and files:

+ `README.md` is this README file.
+ `LICENSE` is the License file.
+ `code_build` is an installation script.
+ `source/` contains the Fortran source code of TEFAL and the Makefile.
+ `bin/` contains the executable after successful installation.
+ `aux/` contains text files and energy grids to be used by TEFAL.
+ `doc/` contains the tutorial in pdf format.
+ `samples/` contains the input and output files of the sample cases, and the *verify* script for the user to run the sample cases.

In total, you will need about 500 Mb of free disk space to install TEFAL.

## License and Copyright
This software is distributed and copyrighted according to the [LICENSE](LICENSE) file.
