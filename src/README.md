# PX913-Project/src
Source code for the project

## Fortran

### GlobalUtils.f90
Defines the RunData, FieldType and ParticleType custom types, which are used in other Fortran modules
Also defines the three initial conditions required for the project : "Null", "Single" and "Double"

### FieldSolver.f90
Uses the Gauss-Seidel method to iteratively solve for the Electric Potential given the charge distribution, and hence find the Electric Field.
This is done via the RunData and FieldType types.

### ParticleSolver.f90
Uses a Verlet method to generate a particle trajectory given the Electric Field, and a particle with initial position and velocity

### PIFWriter.f90
NetCDF based Particle In Field (PIF) file writer to save key information, such as the charge density and electric fields, and the particle positions, velocities, and accelerations.

### Main.f90
Uses all other fortran modules to take a command line input, and from it generate all required data and save to a file.

Currently supported command line kwargs are:
problem : string; type of problem to be solved. Currently "Null", "Single" and "Double" are available.
nx : integer; number of grid cells in x direction
ny : integer; number of grid cells in y direction


## Python
### DataVis.py
Loads data for NetCDF file, and provides key visualisation of the system.

### PIFreader.py
Contains PIFreader class to enable .pif files to very easily be read.


## UNORIGINAL CODE
The following files contain code taken with permission

### command_line.f90
command_line.f90 is written by H Ratcliffe, and is used to parse command line arguments in main.f90

### create_axis.f90
create_axis.f90 was supplied with the project brief, and contains a subroutine to create discretised axes with the correct range and step. This is used to generate axes for the file header, and to link the discretised field coordinates with the continuous particle coordinates.