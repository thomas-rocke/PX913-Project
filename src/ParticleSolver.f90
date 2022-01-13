! Module that uses verlet method to find the trajectory of electrons
! Given electric field, and initial position and velocity

module ParticleSolver

    use ISO_FORTRAN_ENV
    use GlobalUtils    
    implicit none
    
    contains

    subroutine VelocityVerlet(run_data,particle,fields)
        ! Velocity Verlet integrator
        type(RunData), intent(in) :: run_data
        type(ParticleType), intent(inout) :: particle
        type(FieldType), intent(in) :: fields
        integer, parameter :: x = 1, y = 2
        integer :: t
        real(real64), dimension(2) :: E
        
        do t = 0, run_data%numTimesteps -1

            E = fields%E(particle%pos(t,:))
            ! Update particle position
            particle%pos(t+1,x) = particle%pos(t,x) &
                                + particle%vel(t,x)*run_data%dt &
                                + 0.5_REAL64*(particle%acc(t,x))*(run_data%dt**2)
            particle%pos(t+1,y) = particle%pos(t,y) &
                                + particle%vel(t,y)*run_data%dt &
                                + 0.5_REAL64*(particle%acc(t,y))*((run_data%dt)**2)

            ! Get electric field at updated position
            ! Update particle accelrations
            E = fields%E(particle%pos(t,:))
            particle%acc(t+1,x) = E(x)
            particle%acc(t+1,y) = E(y)
         
            ! Update particle velocities
            particle%vel(t+1,x) = particle%vel(t,x) &
                                + ((particle%acc(t+1,x) &
                                +particle%acc(t,x))/2.0_REAL64)*run_data%dt
            particle%vel(t+1,y) = particle%vel(t,y) &
                                + ((particle%acc(t+1,y) &
                                +particle%acc(t,y))/2.0_REAL64)*run_data%dt
        
            print *, "Iteration number: ", t
            print *, "Current Position: "
            print *, particle%pos(t+1,x), particle%pos(t+1,x)
            print *, " "

            print *, "Current Velocity: "
            print *, particle%vel(t+1,x), particle%vel(t+1,x)
            print *, " "

            print *, "Current Acceleration: "
            print *, particle%acc(t,x), particle%acc(t,x)
            print *, "###########################################"

        end do

    end subroutine VelocityVerlet
  
end module ParticleSolver
