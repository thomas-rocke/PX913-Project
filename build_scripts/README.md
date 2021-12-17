# Build Scripts
Scripts to compile code and run various useful routines

CheckWarnings compiles code and saves sterr (all compiler errors and warnings) to Warnings.txt

Memtest compiles code and uses valgrind leak-check to check memory is properly released. stout is dumped in MemTest.txt.

Profile compiles and profiles code, dumping the profile in profile.txt. program main (in Main.f90) is profiled.