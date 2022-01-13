# Build Scripts
Scripts to compile code and run various useful routines
All scripts have nx and ny variables, which change the size of the x and y axes used to define the fields


## Main Build Scripts
BuildOne runs only the problem given by the problem variable in the script.
A new figure is generated only for this problem

BuildAll runs all problems, and generates all figures.

## Debugging Build Scripts

CheckWarnings compiles code and saves sterr (all compiler errors and warnings) to Warnings.txt

Memtest compiles code and uses valgrind leak-check to check memory is properly released. stout is dumped in MemTest.txt.

Profile compiles and profiles code, dumping the profile in profile.txt. program main (in Main.f90) is profiled.
