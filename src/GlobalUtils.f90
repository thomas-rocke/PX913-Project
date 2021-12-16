! Define some useful types and procedures, for use throughout the project

! Particle and FieldData custom types
! Methods to Construct these types based on specific desired initial states

module GlobalUtils
  use ISO_FORTRAN_ENV
  use domain_tools
  implicit none
  
  private
  public RunData, ParticleType, FieldType, SelectConditions

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
    contains
    procedure :: E => Get_E_At_Pos
  end type

  ! ####################
  ! # GLOBAL VARIABLES #
  ! ####################


  contains

  ! #################################
  ! # INITIAL CONDITIONS PROCEDURES #
  ! #################################

  subroutine SelectConditions(requestedState, particle, fields, run_data)
    ! Selects which initialisation subroutine to use to initialise particle and fields
    character(len=*), intent(in) :: requestedState
    type(ParticleType), intent(inout) :: particle
    type(FieldType), intent(inout) :: fields
    type(runData), intent(in) :: run_data

    select case (to_upper(requestedState))
      case ("NULL")
        call NullInitial(particle, fields, run_data)
      
      case ("SINGLE")
        call SingleInitial(particle, fields, run_data)
      
      case default
        print *, "ERROR: '", requestedState, "' not a recognised initial state"
        stop
    end select
  end subroutine

  subroutine SingleInitial(particle, fields, run_data)
    ! Initialises the input Particle and Fields objects based on "Single" initial condition specification
    type(ParticleType), intent(inout) :: particle
    type(FieldType), intent(inout) :: fields
    type(runData), intent(in) :: run_data

    integer :: nx, ny, num_timesteps, i, j
    real(kind=REAL64) :: x, y, x_exp, y_exp

    nx = run_data%nx
    ny = run_data%ny
    num_timesteps = run_data % numTimesteps

    call CleanAndAllocate(particle, fields, nx, ny, num_timesteps)

    ! Initial conditions on the particle
    particle%pos(0, :) = (/0.1_REAL64, 0.0_REAL64/)

    ! Initial conditions on rho
    ! rho(x, y) = exp(-(x/0.1)^2 - (y/0.1)^2)
    !$omp parallel do private(i, x, y, x_exp, y_exp) shared(fields)
    do j=1, ny
      y = j*fields%dy - 1.0_REAL64
      y_exp = exp(-(100.0_REAL64 * y * y))
      do i=1, nx
        x = i*fields%dx - 1.0_REAL64
        x_exp = exp(-(100.0_REAL64 * x * x))
        fields%rho(i, j) = x_exp * y_exp
      end do
    end do

  end subroutine

  subroutine NullInitial(particle, fields, run_data)
    ! Initialises the input Particle and Fields objects based on "Null" initial condition specification
    type(ParticleType), intent(inout) :: particle
    type(FieldType), intent(inout) :: fields
    type(runData), intent(in) :: run_data

    integer :: nx, ny, num_timesteps

    nx = run_data%nx
    ny = run_data%ny
    num_timesteps = run_data%numTimesteps


    call CleanAndAllocate(particle, fields, nx, ny, num_timesteps)
    
    ! Keep all field attributes set to zero everywhere
    ! Set particle intitial velocity to be (0.1, 0.1)
    particle%vel(0, :) = (/0.1_REAL64, 0.1_REAL64/)
    
  end subroutine

  ! #######################
  ! # E FIELD AT POSITION #
  ! #######################

  function Get_E_At_Pos(fields, position) result(E_field)
    ! Gets the electric field strength (E_x, E_y) at position (x, y)
    ! Given the fields object
    ! ASSUMES SPATIAL DOMAIN OF (-1.0, 1.0) IN BOTH X AND Y
    class(FieldType), intent(in) :: fields
    real(kind=REAL64), dimension(2), intent(in) :: position
    real(kind=REAL64), dimension(2) :: E_field
    integer :: x_index, y_index

    ! Default behaviour
    E_field = 0.0_REAL64
    
    if (abs(position(1)) <= 1.0_REAL64 .AND. abs(position(2)) <= 1.0_REAL64) then
      ! position is within the defined E field domain
    
      ! Get closest indeces corresponding to position on grid
      x_index = floor((position(1) - 1.0_REAL64)/fields%dx) + 1
      y_index = floor((position(2) - 1.0_REAL64)/fields%dy) + 1

      ! Populate E_field with closest Ex and Ey grid cells
      E_field(1) = fields%Ex(x_index, y_index)
      E_field(2) = fields%Ey(x_index, y_index)
    end if
  end function

  ! ######################################
  ! # VARIABLE INITIALISATION SUBROUTINE #
  ! ######################################
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


  ! ###################
  ! # MISC PROCEDURES #
  ! ###################

  function to_upper(lower) result(upper)
    ! Converts character string to upper case
    ! Via ASCII representation
    ! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html
    ! Original author: Clive Page
    character(len=*), intent(in) :: lower
    character(len=len(lower)) :: upper
    integer :: i, int_i ! integer representation of the ith character
    integer, parameter :: int_a = iachar("a"), int_z = iachar("z")

    upper = lower
    do i = 1, len(lower)
      int_i = iachar(lower(i:i))
      ! Test if ith character is between a-z
      if (int_i>= int_a .and. int_i<=int_z ) then
        ! Uppercase ASCII is 32 characters from lowercase
        upper(i:i) = achar(int_i-32)
      end if
    end do
  end function to_upper

end module GlobalUtils
