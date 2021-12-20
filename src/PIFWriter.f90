! NetCDF writer class for writing Electric Field and Particle Path properties

module PIFWriter
  use NetCDF
  use GlobalUtils
  implicit none


  ! ####################
  ! # MODULE VARIABLES #
  ! ####################
  character(len=*), parameter :: FILE_START = "Run(", FILE_EXTENSION = ").pif", PATH = "../data/"


  contains
  
  subroutine OpenFile(Run_Data, id)
    ! Creates and opens a file with standard filename
    ! "Run({problem}, nx={nx}, ny={ny}).pif"
    ! Sets id to be the file identifier
    type(RunData), intent(in) :: Run_Data
    integer, intent(out) :: id
    integer :: ierr
    character(len=99) :: fname 
    fname = PATH // FILE_START // trim(Run_Data%problem) // ", nx=" // trim(str(Run_Data%nx)) // &
                  ", ny=" // trim(str(Run_Data%ny)) // FILE_EXTENSION

    !
    ! OPEN FILE
    !

    Print *, "Creating file ", fname

    ierr = nf90_create(trim(fname), NF90_CLOBBER, id)

    ! Print the error, if any.
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF


  end subroutine

  subroutine MakeMetaData(lun, Run_Data, Fields, Particle)
    ! Generates the metadata in the file given by lun
    integer, intent(in) :: lun
    type(RunData), intent(in) :: Run_Data
    type(FieldType), intent(in) :: Fields
    type(ParticleType), intent(in) :: Particle
  end subroutine

  subroutine WriteFields(lun, Fields)
    ! Writes all Fields data to the file
    integer, intent(in) :: lun
    type(FieldType), intent(in) :: Fields
  end subroutine

  subroutine WriteParticle(lun, Particle)
    ! Writes all particle data to the file
    integer, intent(in) :: lun
    type(ParticleType), intent(in) :: Particle
  end subroutine

  subroutine CloseFile(lun)
    ! Closes file given by lun
    integer, intent(in) :: lun
  end subroutine

  function str(i)
    ! Converts integer i to string
    integer, intent(in) :: i
    character(len=99) :: str
    write (str, *) i
    str = adjustl(str)
  end function
end module PIFWriter
