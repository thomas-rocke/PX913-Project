! Define some useful types and procedures, for use throughout the project

! Particle and FieldData custom types
! Methods to Construct these types based on specific desired initial states

module GlobalUtils
  use ISO_FORTRAN_ENV
  implicit none
  
  private
  public RunData, ParticleType, FieldType, NullInitial

  ! ################
  ! # CUSTOM TYPES #
  ! ################

  type :: RunData
    integer :: nx, ny, numTimesteps

  end type

  type :: ParticleType
    ! Store position, velocity, and acceleration vectors as 2D arrays
    ! positions(i, j) gives the jth component of particle position at timestep i 
    real(kind=REAL64), dimension(:, :), allocatable :: positions, velocities, accelerations
  end type

  type :: FieldType
    ! Store 2D arrays of the charge density, electric potential, and both x- and y-components of the Electric Field
    real(kind=REAL64), dimension(:, :), allocatable :: chargeDensity, electricPotential, Ex, Ey
  end type

  ! ####################
  ! # GLOBAL VARIABLES #
  ! ####################


  contains

  subroutine NullInitial(particle, fields, run_data)
    ! Initialises the input Particle and Fields objects based on "Null" initial condition specification
    type(ParticleType), intent(inout) :: particle
    type(FieldType), intent(inout) :: fields
    type(runData), intent(in) :: run_data

    integer :: nx, ny, num_timesteps

    nx = run_data%nx
    ny = run_data%ny
    num_timesteps = run_data % numTimesteps


    call CleanAndAllocate(particle, fields, nx, ny, num_timesteps)
    
    ! Keep all field attributes set to zero everywhere
    ! Set particle intitial velocity to be (0.1, 0.1)
    particle%velocities(0, 0) = 0.1_REAL64
    particle%velocities(0, 1) = 0.1_REAL64
    
  end subroutine

  subroutine CleanAndAllocate(particle, fields, nx, ny, num_timesteps)
    ! Deallocates particle and fields type attributes (positions, chargeDensity, velocities, ...)
    ! And Allocates all attributes to the correct size

    type(ParticleType), intent(inout) :: particle
    type(FieldType), intent(inout) :: fields

    integer, intent(in) :: nx, ny, num_timesteps
    
    ! Clean any allocations in Particle object
    if (allocated(particle%positions)) deallocate(particle%positions)
    if (allocated(particle%velocities)) deallocate(particle%velocities)
    if (allocated(particle%accelerations)) deallocate(particle%accelerations)

    ! Clean any allocations in the fields object
    if (allocated(fields%chargeDensity)) deallocate(fields%chargeDensity)
    if (allocated(fields%electricPotential)) deallocate(fields%electricPotential)
    if (allocated(fields%Ex)) deallocate(fields%Ex)
    if (allocated(fields%Ey)) deallocate(fields%Ey)

    ! Particle object allocation
    allocate(particle%positions(0:num_timesteps, 2))
    allocate(particle%velocities(0:num_timesteps, 2))
    allocate(particle%accelerations(0:num_timesteps, 2))

    ! Fields object allocation
    allocate(fields%chargeDensity(0:nx+1, 0:ny+1))
    allocate(fields%electricPotential(0:nx+1, 0:ny+1))
    allocate(fields%Ex(0:nx+1, 0:ny+1))
    allocate(fields%Ey(0:nx+1, 0:ny+1))

    ! Particle object defaults
    particle%positions = 0_REAL64
    particle%velocities = 0_REAL64
    particle%accelerations = 0_REAL64

    ! Fields object defaults
    fields%chargeDensity = 0_REAL64
    fields%electricPotential = 0_REAL64
    fields%Ex = 0_REAL64
    fields%Ey = 0_REAL64
  end subroutine

end module GlobalUtils


program testGlobals
  implicit none

end program testGlobals