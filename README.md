# PX913-Project
HetSys PX913 Group Project Repo

## Fortran
### GlobalUtils.f90
Defines the RunData, Field and Particle custom types, which are used in other Fortran modules
Also defines the three initial conditions required for the project : "Null", "Single" and "Double"

### FieldSolver.f90
Uses the Gauss-Seidel method to iteratively solve for the Electric Potential given the charge distribution, and hence find the Electric Field.

### ParticleSolver.f90
Uses a Verlet method to generate a particle trajectory given the Electric Field, and a particle with initial position and velocity

### PathWriter.f90
NetCDF based file writer to save key information, such as the charge density and electric fields, and the particle positions, velocities, and accelerations.

### Main.f90
Uses all other fortran modules to take a command line input, and from it generate all required data and save to a file.

Currently supported command line kwargs are:


## Python
### DataVis.py
Loads data for NetCDF file, and provides key visualisation of the system.
