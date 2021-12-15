! Main program to parse command line arguments and run the electric field and particle path solvers

program main
  use ISO_FORTRAN_ENV
  use GlobalUtils
  use FieldSolver
  use ParticleSolver
  use PIFWriter

  implicit none

  type(RunData) :: Run_Data
  type(FieldType) :: Fields
  type(ParticleType) :: particle

  Run_Data%nx = 100
  Run_Data%ny = 100
  Run_Data%numTimesteps = 1000
  
  call SingleInitial(particle, Fields, Run_Data)
  call Get_Field(Fields)
  
  Print *, Fields%dx




end program main
