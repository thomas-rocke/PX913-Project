! Main program to parse command line arguments and run the electric field and particle path solvers

program main
  use ISO_FORTRAN_ENV
  use GlobalUtils
  use FieldSolver
  use ParticleSolver
  use PIFWriter
  use command_line

  implicit none

  type(RunData) :: Run_Data
  type(FieldType) :: Fields
  type(ParticleType) :: particle

  character(len=10) :: sysType
  logical :: nxPresent, nyPresent, typePresent


  call parse_args() ! Get all command line args

  ! Try to get "nx=" arg
  nxPresent = get_arg("nx", Run_Data%nx)

  nyPresent = get_arg("ny", Run_Data%ny)

  typePresent = get_arg("problem", sysType)

  if (.NOT. (nxPresent .AND. nyPresent .AND. typePresent)) then
    if (.NOT. nxPresent) print *, "ERROR: Command line argument nx not found"
    if (.NOT. nyPresent) print *, "ERROR: Command line argument ny not found"
    if (.NOT. typePresent) print *, "ERROR: Command line argument problem not found"
    print *, "ERROR: Required args missing, aborting"
    stop
  end if

  call SelectConditions("single", particle, Fields, Run_Data)
  call Get_Field(Fields)
  
  Print *, Fields%E((/0.0_REAL64, 3.0_REAL64/))




end program main
