#PX913-Project/src
Source code for the project

## Fortran

### GlobalUtils.f90
Defines the RunData, Field and Particle custom types, which are used in other Fortran modules
Also defines the three initial conditions required for the project : "Null", "Single" and "Double"

### FieldSolver.f90
Uses the Gauss-Seidel method to iteratively solve for the Electric Potential given the charge distribution, and hence find the Electric Field.

### ParticleSolver.f90
Uses a Verlet method to generate a particle trajectory given the Electric Field, and a particle with initial position and velocity

### PIFWriter.f90
NetCDF based file writer to save key information, such as the charge density and electric fields, and the particle positions, velocities, and accelerations.

### Main.f90
Uses all other fortran modules to take a command line input, and from it generate all required data and save to a file.

Currently supported command line kwargs are:


## Python
### DataVis.py
Loads data for NetCDF file, and provides key visualisation of the system.

### PIFreader.py
Contains PIFreader class to enable .pif files to very easily be read.

###Logging.py
Utilities for generating log files during python runtime


## UNORIGINAL CODE
The following files contain code taken with permission/within license requirements

### command_line.f90
command_line.f90 is written by H Ratcliffe, and is used to parse command line arguments in main.f90

### create_axis.f90
create_axis.f90 was supplied with the project brief, and contains a subroutine to create discretised axes with the correct range and step. This is used in PIFwriter.f90 to generate the correct axes in the file header.

### logging.f90
logging.f90 is written by Chris MacMackin on the flogging GitHub repository (https://github.com/cmacmackin/flogging), and is used to generate and populate logging files for use in debugging.
