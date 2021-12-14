! Define some useful types and procedures, for use throughout the project

! Particle and FieldData custom types
! Methods to Construct these types based on specific desired initial states

module GlobalUtils
  use ISO_FORTRAN_ENV
  use domain_tools
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
    real(kind=REAL64), dimension(:, :), allocatable :: pos, vel, acc
  end type

  type :: FieldType
    ! Store 2D arrays of the charge density (rho), electric potential (phi), and both x- and y-components of the Electric Field
    real(kind=REAL64), dimension(:, :), allocatable :: rho, phi, Ex, Ey
    ! Store arrays of the axes
    real(kind=REAL64), dimension(:), allocatable :: x_axis, y_axis
    ! Store the grid spacings in x and y
    real(kind=REAL64) :: dx, dy
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
    particle%vel(0, 0) = 0.1_REAL64
    particle%vel(0, 1) = 0.1_REAL64
    
  end subroutine

  subroutine CleanAndAllocate(particle, fields, nx, ny, num_timesteps)
    ! Deallocates particle and fields type attributes (positions, charge density, velocities, ...)
    ! And Allocates all attributes to the correct size

    type(ParticleType), intent(inout) :: particle
    type(FieldType), intent(inout) :: fields

    integer, intent(in) :: nx, ny, num_timesteps
    
    ! Clean any allocations in Particle object
    if (allocated(particle%pos)) deallocate(particle%pos)
    if (allocated(particle%vel)) deallocate(particle%vel)
    if (allocated(particle%acc)) deallocate(particle%acc)

    ! Clean any allocations in the fields object
    if (allocated(fields%rho)) deallocate(fields%rho)
    if (allocated(fields%phi)) deallocate(fields%phi)
    if (allocated(fields%Ex)) deallocate(fields%Ex)
    if (allocated(fields%Ey)) deallocate(fields%Ey)
    if (allocated(fields%x_axis)) deallocate(fields%x_axis)
    if (allocated(fields%y_axis)) deallocate(fields%y_axis)

    ! Particle object allocation
    allocate(particle%pos(0:num_timesteps, 2))
    allocate(particle%vel(0:num_timesteps, 2))
    allocate(particle%acc(0:num_timesteps, 2))

    ! Fields object allocation
    allocate(fields%rho(0:nx+1, 0:ny+1))
    allocate(fields%phi(0:nx+1, 0:ny+1))
    allocate(fields%Ex(1:nx, 1:ny))
    allocate(fields%Ey(1:nx, 1:ny))
    
    call create_axis(fields%x_axis, nx, (/-1.0_REAL64, 1.0_REAL64/), delta=fields%dx)
    call create_axis(fields%y_axis, ny, (/-1.0_REAL64, 1.0_REAL64/), delta=fields%dy)

    ! Particle object defaults
    particle%pos = 0_REAL64
    particle%vel = 0_REAL64
    particle%acc = 0_REAL64

    ! Fields object defaults
    fields%rho = 0_REAL64
    fields%phi = 0_REAL64
    fields%Ex = 0_REAL64
    fields%Ey = 0_REAL64
  end subroutine

end module GlobalUtils
