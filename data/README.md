# Data Files
Directory to store all generated data files

## File Name Scheme
Files should follow the following naming scheme:
"Run({problem}, nx={nx}, ny={ny}).pif"

## .pif File Format Specifications
.pif files are NetCDF encoded files.
Defined variables are as follows:

### File Header Variables
"problem" : string; type of problem - one of {"Null", "Single", "Double"}
"nx" : integer; number of grid points in the x direction
"ny" : integer; number of grid points in the y direction
"dx" : REAL64; displacement between adjacent grid points in the x direction
"dy" : REAL64; displacement between adjacent grid points in the y direction
"timesteps" : integer; number of timesteps in the particle trajectory
"dt" : REAL64; The elapsed time between timesteps

### Axes
"x_axis" : REAL64 array, len=nx; x positions of all grid points
"y_axis" : REAL64 array, len=ny; y positions of all grid points
"time" : REAL64 array, len=Timesteps; Total elapsed time at each timestep
"xy_axis" : character array, len=2; Equal to (/"x", "y"/), used to define the particle arrays

### Field Data
"ChargeDensity" : (1:nx, 1:ny) matrix of REAL64s; Charge density at all grid points
"ElectricPotential" : (1:nx, 1:ny) matrix of REAL64s; Electric Potential Field at all points. grad^2 ElectricPotential = ChargeDensity.
"E_x" : (1:nx, 1:ny) matrix of REAL64s; x component of the Electrostatic Force Field. Equal to x component of grad of ElectricPotential.
"E_y" : (1:nx, 1:ny) matrix of REAL64s; y component of the Electrostatic Force Field. Equal to y component of grad of ElectricPotential.

### Particle Data
Each of the Particle Data matrices encode data for each timestep, so Prop(i, :) gets the (Prop_x, Prop_y) vector at timestep i.
Timestep 0 is the particle initial conditions

"Positions" : (0:timesteps, 1:2) matrix of REAL64s; (x, y) positions at each timestep.
"Velocities" : (0:timesteps, 1:2) matrix of REAL64s; (v_x, v_y) velocities at each timestep.
"Accelerations" : (0:timesteps, 1:2) matrix of REAL64s; (a_x, a_y) accelerations at each timestep.

