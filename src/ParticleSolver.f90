! Module that uses verlet method to find the trajectory of electrons
! Given electric field, and initial position and velocity


module ParticleSolver

    use ISO_FORTRAN_ENV
    use GlobalUtils    
    implicit none
    
    contains

    subroutine init_pos_vel(sysType,run_data,particle,fields)

        type(RunData), intent(in) :: run_data
        type(ParticleType), intent(inout) :: particle
        type(FieldType), intent(in) :: fields
        character(len=10) :: sysType
        integer :: x, y 

        ! Set initial conditions
        if (run_data%problem .eq. "null") then

            particle%pos(1,1) = 0.0_REAL64
            particle%pos(1,2) = 0.0_REAL64
            
            particle%vel(1,1) = 0.1_REAL64
            particle%vel(1,2) = 0.1_REAL64

        else if (run_data%problem .eq. "single") then

            particle%pos(1,1) = 0.1_REAL64
            particle%pos(1,2) = 0.0_REAL64
            
            particle%vel(1,1) = 0.0_REAL64
            particle%vel(1,2) = 0.0_REAL64

        else if (run_data%problem .eq. "double") then

            particle%pos(1,1) = 0.0_REAL64
            particle%pos(1,2) = 0.5_REAL64
            
            particle%vel(1,1) = 0.0_REAL64
            particle%vel(1,2) = 0.0_REAL64
        
        else
            print *, "Invalid initial condition"
            print *, "Stopping"
            STOP
        end if

        ! Determine initial cell
        x = floor((particle%pos(1,1)+1.0_REAL64)/fields%dx) + 1
        y = floor((particle%pos(1,2)+1.0_REAL64)/fields%dy) + 1

        print *, x
        print *, y

        particle%acc(1,1) = fields%Ex(x,y)
        particle%acc(1,2) = fields%Ey(x,y)

    
        print *, "Initial Position: "
        print *, "x: ", particle%pos(1,1)
        print *, "y: ", particle%pos(1,2)
    
        print *, "Initial Velocity: "
        print *, "x: ", particle%vel(1,1)
        print *, "y: ", particle%vel(1,2)

        print *, "Initial Acceleration: "
        print *, "x: ", particle%acc(1,1)
        print *, "y: ", particle%acc(1,2)

    end subroutine init_pos_vel

    subroutine VelocityVerlet(run_data,particle,fields)
        type(RunData), intent(in) :: run_data
        type(ParticleType), intent(inout) :: particle
        type(FieldType), intent(in) :: fields
        integer, parameter :: x = 1, y = 2
        integer :: t
        real(kind=real64), dimension(2) :: E
        
        do t = 0, run_data%numTimesteps -1

            E = fields%E(particle%pos(t,:))

            ! x and y position, acceleration, and velocity updates

            particle%pos(t+1,x) = particle%pos(t,x) &
                                + particle%vel(t,x)*run_data%dt &
                                + 0.5_REAL64*(particle%acc(t,x))*(run_data%dt**2)
            particle%pos(t+1,y) = particle%pos(t,y) &
                                + particle%vel(t,y)*run_data%dt &
                                + 0.5_REAL64*(particle%acc(t,y))*((run_data%dt)**2)

            !x = floor((particle%pos(t+1,x)+1.0_REAL64)/fields%dx) + 1 ! Determine the new cell the particle is in (at t+1)
            !y = floor((particle%pos(t+1,y)+1.0_REAL64)/fields%dy) + 1 
            
            E = fields%E(particle%pos(t,:))

            particle%acc(t+1,x) = E(x)
            particle%acc(t+1,y) = E(y)
            ! particle%acc(t+1,x) = fields%Ex(x,y) ! E(x)
            ! particle%acc(t+1,y) = fields%Ey(x,y) ! E(y)

            particle%vel(t+1,x) = particle%vel(t,x) &
                                + ((particle%acc(t+1,x) &
                                +particle%acc(t,x))/2.0_REAL64)*run_data%dt
            particle%vel(t+1,y) = particle%vel(t,y) &
                                + ((particle%acc(t+1,y) &
                                +particle%acc(t,y))/2.0_REAL64)*run_data%dt
        
            print *, "Iteration number: ", t
            print *, "Current Position: "
            print *, "x: ", particle%pos(t+1,x)
            print *, "y: ", particle%pos(t+1,x)
            print *, " "

            print *, "Current Velocity: "
            print *, "x: ", particle%vel(t+1,x)
            print *, "y: ", particle%vel(t+1,x)
            print *, " "

            print *, "Current Acceleration: "
            print *, "x: ", particle%acc(t,x)
            print *, "y: ", particle%acc(t,x)
            print *, "#####################"

        end do

    end subroutine VelocityVerlet
  
end module ParticleSolver
