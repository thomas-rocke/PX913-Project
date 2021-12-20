! NetCDF writer class for writing Electric Field and Particle Path properties

module PIFWriter
  !use NetCDF
  use ISO_FORTRAN_ENV
  use GlobalUtils
  implicit none


  type :: FileData
    integer :: file_id, x_axis_id, y_axis_id, time_axis_id, &
          rho_id, phi_id, ex_id, ey_id, pos_id, vel_id, acc_id, xy_axis_id
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
  character(len=*), parameter :: XAXIS_NAME = "x_axis", YAXIS_NAME = "y_axis", TAXIS_NAME = "time", XY_AXIS_NAME = "xy_axis"
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
    integer :: ierr, x_axis_id, y_axis_id, id, i
    real(kind=REAL64), dimension(0:Run_Data%numTimesteps) :: t_axis

    id = File_Data%file_id

    ! RUN_DATA GLOBALS

    ! problem
    call make_global(PROBLEM_NAME, Run_Data%problem, id)

    ! nx
    call make_global(NX_NAME, Run_Data%nx, id)

    ! ny
    call make_global(NY_NAME, Run_Data%ny, id)

    ! dx
    call make_global(DX_NAME, Fields%dx, id)

    ! dy
    call make_global(DY_NAME, Fields%dy, id)

    ! timesteps
    call make_global(TSTEP_NAME, Run_Data%numTimesteps, id)

    ! dt
    call make_global(DT_NAME, Run_Data%dt, id)

    ! AXES
    ! x_axis
    ierr = nf90_def_dim(id, XAXIS_NAME, Run_Data%nx, File_Data%x_axis_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! y_axis
    ierr = nf90_def_dim(id, YAXIS_NAME, Run_Data%ny, File_Data%y_axis_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! time_axis
    ierr = nf90_def_dim(id, TAXIS_NAME, Run_Data%numTimesteps + 1, File_Data%time_axis_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! xy_axis
    ierr = nf90_def_dim(id, XY_AXIS_NAME, 2, File_Data%xy_axis_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! FIELDS MATRICES
    
    ! rho
    ierr = nf90_def_var(id, RHO_NAME, NF90_REAL64, (/ File_Data%x_axis_id, &
                        File_Data%y_axis_id /), File_Data%rho_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! phi
    ierr = nf90_def_var(id, PHI_NAME, NF90_REAL64, (/ File_Data%x_axis_id, &
                        File_Data%y_axis_id /), File_Data%phi_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! Ex
    ierr = nf90_def_var(id, EX_NAME, NF90_REAL64, (/ File_Data%x_axis_id, &
                        File_Data%y_axis_id /), File_Data%ex_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! Ey
    ierr = nf90_def_var(id, EY_NAME, NF90_REAL64, (/ File_Data%x_axis_id, &
                        File_Data%y_axis_id /), File_Data%ey_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! PARTICLE ARRAYS
    ! Pos
    ierr = nf90_def_var(id, POS_NAME, NF90_REAL64, (/ File_Data%time_axis_id, &
                        File_Data%xy_axis_id /), File_Data%pos_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! Vel
    ierr = nf90_def_var(id, VEL_NAME, NF90_REAL64, (/ File_Data%time_axis_id, &
                        File_Data%xy_axis_id /), File_Data%vel_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! Acc
    ierr = nf90_def_var(id, ACC_NAME, NF90_REAL64, (/ File_Data%time_axis_id, &
                        File_Data%xy_axis_id /), File_Data%acc_id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! End NetCDF variable definitions
    ierr = nf90_enddef(id)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! Populate Time axis
    t_axis = 0.0_REAL64

    do i=1, Run_Data%numTimesteps
      t_axis(i) = i * Run_Data%dt
    end do

    ierr = nf90_put_var(id, File_Data%time_axis_id, t_axis)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! Populate xy axis
    ierr = nf90_put_var(id, File_Data%xy_axis_id, (/"x", "y"/))
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF
  end subroutine

  subroutine WriteFields(File_Data, Fields)
    ! Writes all Fields data to the file
    type(FileData), intent(in) :: File_Data
    type(FieldType), intent(in) :: Fields
    integer :: ierr, id
    id = File_Data%file_id

    ! FILL IN AXES

    ! x_axis
    ierr = nf90_put_var(id, File_Data%x_axis_id, Fields%x_axis)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! y_axis
    ierr = nf90_put_var(id, File_Data%y_axis_id, Fields%y_axis)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF
     
    ! FILL IN MATRICES

    ! rho
    ierr = nf90_put_var(id, File_Data%rho_id, Fields%rho)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! phi
    ierr = nf90_put_var(id, File_Data%phi_id, Fields%phi)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! E_x
    ierr = nf90_put_var(id, File_Data%Ex_id, Fields%Ex)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! E_y
    ierr = nf90_put_var(id, File_Data%Ey_id, Fields%Ey)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

  end subroutine

  subroutine WriteParticle(File_Data, Particle)
    ! Writes all particle data to the file
    type(FileData), intent(in) :: File_Data
    type(ParticleType), intent(in) :: Particle
    integer :: ierr, id
    id = File_Data%file_id

    ! Pos
    ierr = nf90_put_var(id, File_Data%Pos_id, Particle%pos)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! Vel
    ierr = nf90_put_var(id, File_Data%Vel_id, Particle%vel)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ! Acc
    ierr = nf90_put_var(id, File_Data%Acc_id, Particle%acc)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF
  end subroutine

  subroutine CloseFile(File_Data)
    ! Closes file given by id
    type(FileData), intent(in) :: File_Data
    integer :: ierr, id
    id = File_Data%file_id
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
    integer :: ierr

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
    integer :: ierr

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
    integer :: ierr

    ierr = nf90_put_att(file_id, NF90_GLOBAL, key, val)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF
  end subroutine
end module PIFWriter
