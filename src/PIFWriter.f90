! NetCDF writer class for writing Electric Field and Particle Path properties

module PIFWriter
  implicit none
  !use NetCDF
  use GlobalUtils
  contains
  
  subroutine OpenFile(filename, lun)
    ! Creates and opens a file with given filename
    ! Sets lun to be the file identifier
    character(len=*), intent(in) :: filename
    integer, intent(out) :: lun
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
end module PIFWriter
