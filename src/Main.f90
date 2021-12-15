! Main program to parse command line arguments and run the electric field and particle path solvers

program main
  use ISO_FORTRAN_ENV
  use GlobalUtils
  use FieldSolver
  use ParticleSolver
  use PathWriter

  implicit none

  type(RunData) :: Run_Data
  type(FieldType) :: Fields
  type(ParticleType) :: particle

  Run_Data%nx = 10
  Run_Data%ny = 10
  
  call NullInitial(particle, Fields, Run_Data)
  call Get_Field(Fields)
  
  Print *, Fields%E((/0.0_REAL64, 0.0_REAL64/))




end program main
