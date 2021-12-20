! NetCDF writer class for writing Electric Field and Particle Path properties

module PIFWriter
  use NetCDF
  use GlobalUtils
  implicit none


  type :: FileData
    integer :: file_id, x_axis_id, y_axis_id, time_axis_id, %
          rho_id, phi_id, pos_id, vel_id, acc_id
  end type

  ! ####################
  ! # MODULE VARIABLES #
  ! ####################
  character(len=*), parameter :: FILE_START = "Run(", FILE_EXTENSION = ").pif", PATH = "../data/"


  ! #######################
  ! # FILE VARIABLE NAMES #
  ! #######################
  
  ! Run_Data
  character(len=*), parameter :: PROBLEM_NAME = "problem", NX_NAME = "nx", NY_NAME = "ny", &
          DX_NAME = "dx", DY_NAME = "dy", TSTEP_NAME = "timesteps", DT_NAME = "dt"
  
  ! Axes
  character(len=*), parameter :: XAXIS_NAME = "x_axis", YAXIS_NAME = "y_axis", TAXIS_NAME = "time"
  ! Fields
  character(len=*), parameter :: RHO_NAME = "ChargeDensity", PHI_NAME = "ElectricPotential", &
          EX_NAME = "E_x", EY_NAME = "E_y"

  ! Particle
  character(len=*), parameter :: POS_NAME = "Positions", VEL_NAME = "Velocities", ACC_NAME = "Accelerations"

  interface make_global
    procedure make_global_int
    procedure make_global_real
    procedure make_global_char

  end interface


  contains
  
  subroutine OpenFile(Run_Data, File_Data)
    ! Creates and opens a file with standard filename
    ! "Run({problem}, nx={nx}, ny={ny}).pif"
    ! Sets id to be the file identifier
    type(RunData), intent(in) :: Run_Data
    type(FileData), intent(out) :: File_Data
    integer :: ierr
    character(len=99) :: fname 
    fname = PATH // FILE_START // trim(Run_Data%problem) // ", nx=" // trim(str(Run_Data%nx)) // &
                  ", ny=" // trim(str(Run_Data%ny)) // FILE_EXTENSION

    !
    ! OPEN FILE
    !

    Print *, "Creating file ", fname

    ierr = nf90_create(trim(fname), NF90_CLOBBER, File_Data%file_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF
  end subroutine

  subroutine MakeMetaData(File_Data, Run_Data, Fields, Particle)
    ! Generates the metadata in the file given by id
    type(FileData), intent(inout) :: File_Data
    type(RunData), intent(in) :: Run_Data
    type(FieldType), intent(in) :: Fields
    type(ParticleType), intent(in) :: Particle
    integer :: ierr, x_axis_id, y_axis_id, id

    id = File_Data%file_id

    ! RUN_DATA GLOBALS

    ! problem
    call make_global(PROBLEM_NAME, Run_Data%problem, id)

    ! nx
    call make_global(NX_NAME, Run_Data%nx, id)

    ! ny
    call make_global(NY_NAME, Run_Data%ny, id)

    ! dx
    call make_global(DX_NAME, Run_Data%dx, id)

    ! dy
    call make_global(DY_NAME, Run_Data%dy, id)

    ! timesteps
    call make_global(TSTEP_NAME, Run_Data%timesteps, id)

    ! dt
    call make_global(DT_NAME, Run_Data%dt, id)

    ! AXES
    ! x_axis
    ierr = nf90_def_dim(id, XAXIS_NAME, nx, x_axis_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF



  end subroutine

  subroutine WriteFields(id, Fields)
    ! Writes all Fields data to the file
    integer, intent(in) :: id
    type(FieldType), intent(in) :: Fields
    integer :: ierr
  end subroutine

  subroutine WriteParticle(id, Particle)
    ! Writes all particle data to the file
    integer, intent(in) :: id
    type(ParticleType), intent(in) :: Particle
    integer :: ierr
  end subroutine

  subroutine CloseFile(id)
    ! Closes file given by id
    integer, intent(in) :: id
    integer :: ierr
    ierr = nf90_close(id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF
  end subroutine

  function str(i)
    ! Converts integer i to string
    integer, intent(in) :: i
    character(len=99) :: str
    write (str, *) i
    str = adjustl(str)
  end function

  subroutine make_global_int(key, val, file_id)
    ! Makes global attribute with name key
    character(len=*), intent(in) :: key
    integer, intent(in) :: val, file_id

    ierr = nf90_put_att(file_id, NF90_GLOBAL, key, val)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF
  end subroutine

  subroutine make_global_real(key, val, file_id)
    ! Makes global attribute with name key
    character(len=*), intent(in) :: key
    integer, intent(in) :: file_id
    real(kind=REAL64), intent(in) :: val

    ierr = nf90_put_att(file_id, NF90_GLOBAL, key, val)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF
  end subroutine

  subroutine make_global_char(key, val, file_id)
    ! Makes global attribute with name key
    character(len=*), intent(in) :: key
    integer, intent(in) :: file_id
    character(len=*), intent(in) :: val

    ierr = nf90_put_att(file_id, NF90_GLOBAL, key, val)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF
  end subroutine
end module PIFWriter
