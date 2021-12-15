! Module that uses Gauss-Seidel method to find the Electric field
! Given a charge density field rho



module FieldSolver
  use ISO_FORTRAN_ENV
  use GlobalUtils
  implicit none

  private
  public Get_Field

  ! ####################
  ! # MODULE VARIABLES #
  ! ####################
  real(kind=REAL64), parameter :: DEFAULT_TOLERANCE = 1e-4
  integer, parameter :: DEFAULT_MAX_ITERS = 20000

  contains

  ! ################
  ! # GAUSS-SEIDEL #
  ! ################

  subroutine Get_Field(Fields, tolerance, max_iters)
    ! Uses Gauss-Seidel iteration to converge Fields%phi, such that 
    ! grad^2 Gield_Data%rho = Fields%phi
    ! tolerance is the maximum error tolerance for successful convergence
    ! max_iters is the maximum number of iterations done before
    ! convergence taken as failed 
    type(FieldType), intent(inout) :: Fields
    real(kind=REAL64), optional, intent(in) :: tolerance
    integer, optional, intent(in) :: max_iters
    integer :: N_iters, iter
    real(kind=REAL64) :: inv_dx, inv_dy, inv_dx_square, inv_dy_square, e_tot, d_rms, error, err_tol

    inv_dx = 1.0_REAL64/Fields%dx
    inv_dy = 1.0_REAL64/Fields%dy

    inv_dx_square = inv_dx * inv_dx
    inv_dy_square = inv_dy * inv_dy


    if (present(tolerance)) then
      err_tol = tolerance
    else 
      err_tol = DEFAULT_TOLERANCE
    end if

    if (present(max_iters)) then
      N_iters = max_iters
    else
      N_iters = DEFAULT_MAX_ITERS
    end if
    
    do iter=1,N_iters
      call Gauss_Seidel_Iteration(Fields%phi, Fields%rho, inv_dx_square, inv_dy_square)

      e_tot = get_e_tot(Fields%phi, Fields%rho, inv_dx_square, inv_dy_square)
      d_rms = get_d_rms(Fields%phi, inv_dx_square, inv_dy_square)
      error = abs(e_tot / d_rms)

      Print *, "Iteration ", iter, " finished with error ", error
      if (error <= err_tol) exit ! Abort loop if convergence reached
    end do

    ! Update the electric field components
    call E_x(Fields, inv_dx)
    call E_y(Fields, inv_dy)

  end subroutine


  subroutine Gauss_Seidel_Iteration(phi, rho, inv_dx_square, inv_dy_square)
    ! Performs one iteration of the Gauss-Seidel method to solve phi
    real(kind=REAL64), dimension(:, :), intent(in) :: rho
    real(kind=REAL64), dimension(0:size(rho, 1) + 1, 0:size(rho, 2) + 1), intent(inout) :: phi ! Guarded phi (with BCs)
    real(kind=REAL64), intent(in) :: inv_dx_square, inv_dy_square
    integer :: i, j, n_x, n_y
    real(kind=real64) :: deriv, inv_denom

    inv_denom = 1/(2*inv_dx_square + 2*inv_dy_square)

    n_x = size(rho, 1)
    n_y = size(rho, 2)

    do j=1, n_y
      do i=1,n_x
        ! Compute the 2nd order total derivative of phi at position (i, j)
        deriv = (phi(i+1, j) + phi(i-1, j))*inv_dx_square + (phi(i, j+1) + phi(i, j-1))*inv_dy_square
        phi(i, j) = inv_denom * (deriv - rho(i, j))
      end do
    end do
  end subroutine


  ! ##################
  ! # ELECTRIC FIELD #
  ! ##################

  subroutine E_x(Fields, inv_dx)
    ! Solve the x component of the electric field given the potential
    type(FieldType), intent(inout) :: Fields
    real(kind=REAL64), intent(in) :: inv_dx
    real(kind=REAL64), dimension(0:size(Fields%phi, 1) + 1, &
                                  0:size(Fields%phi, 2) + 1) :: Guarded_phi
    integer :: i, j

    Guarded_phi = 0.0_REAL64

    Guarded_phi(1:size(Fields%phi, 1), 1:size(Fields%phi, 2)) = Fields%phi

    !$omp parallel do private(i) shared(Fields, Guarded_phi)
    do j=1, size(Fields%phi, 2)
      do i=1, size(Fields%phi, 1)
        Fields%Ex(i, j) = (Guarded_phi(i+1, j) - Guarded_phi(i-1, j)) * 0.5_REAL64 * inv_dx
      end do
    end do
  end subroutine

  subroutine E_y(Fields, inv_dy)
    ! Solve the y component of the electric field given the potential
    type(FieldType), intent(inout) :: Fields
    real(kind=REAL64), intent(in) :: inv_dy
    real(kind=REAL64), dimension(0:size(Fields%phi, 1) + 1, &
                                  0:size(Fields%phi, 2) + 1) :: Guarded_phi
    integer :: i, j

    Guarded_phi = 0.0_REAL64

    Guarded_phi(1:size(Fields%phi, 1), 1:size(Fields%phi, 2)) = Fields%phi

    !$omp parallel do private(i) shared(Fields, Guarded_phi)
    do j=1, size(Fields%phi, 2)
      do i=1, size(Fields%phi, 1)
        Fields%Ey(i, j) = (Guarded_phi(i, j+1) - Guarded_phi(i, j-1)) * 0.5_REAL64 * inv_dy
      end do
    end do
  end subroutine

  ! ###################
  ! # ERROR FUNCTIONS #
  ! ###################

  function get_e_tot(phi, rho, inv_dx_square, inv_dy_square) result(e_tot)
    ! Gets a measure of the total accumulated error by taking
    ! sum(abs(grad^2 phi - rho))
    real(kind=REAL64), dimension(:, :), intent(in) :: rho
    real(kind=REAL64), dimension(0:size(rho, 1) + 1, 0:size(rho, 2) + 1), intent(inout) :: phi ! Guarded phi (with BCs)
    real(kind=REAL64), intent(in) :: inv_dx_square, inv_dy_square
    real(kind=REAL64), dimension(size(rho, 1), size(rho, 2)) :: deriv, error
    real(kind=REAL64) :: e_tot
    integer :: i, j, n_x, n_y

    n_x = size(rho, 1)
    n_y = size(rho, 2)

    !$omp parallel do private(i) shared(deriv, phi, inv_dx_square, inv_dy_square)
    do j=1, n_y
      do i=1,n_x
        ! Compute the 2nd order total derivative of phi at position (i, j)
        deriv(i, j) = Total_Deriv_2D(phi(i-1:i+1, j-1:j+1), inv_dx_square, inv_dy_square)
      end do
    end do

    error = deriv - rho
    e_tot = sum(abs(error))
  end function

  function get_d_rms(phi, inv_dx_square, inv_dy_square) result(d_rms)
    ! Computes a characteristic length, d_rms, based on
    ! grad^2 phi, which is averaged across all grid points (on the real grid)
    real(kind=REAL64), dimension(0:, 0:), intent(inout) :: phi ! Guarded phi (with BCs)
    real(kind=REAL64), intent(in) :: inv_dx_square, inv_dy_square
    real(kind=REAL64), dimension(size(phi, 1) - 2, size(phi, 2) - 2) :: deriv
    real(kind=REAL64) :: d_rms
    integer :: i, j, n_x, n_y, N

    n_x = size(phi, 1) - 2
    n_y = size(phi, 2) - 2
    N = n_x * n_y

    !$omp parallel do private(i) shared(deriv, phi, inv_dx_square, inv_dy_square)
    do j=1, n_y
      do i=1,n_x
        ! Compute the 2nd order total derivative of phi at position (i, j)
        deriv(i, j) = Total_Deriv_2D(phi(i-1:i+1, j-1:j+1), inv_dx_square, inv_dy_square)
      end do
    end do

    d_rms = sqrt(abs(sum(deriv)/N))
  end function

  ! ################
  ! # MODULE UTILS #
  ! ################
  
  function Total_Deriv_2D(Neighbour_rarr, inv_dx_square, inv_dy_square) result(deriv)
    ! Computes the 2nd order total derivative of point (i, j) of a matrix
    ! Given the 3x3 Neighbours grid, Neighbour_rarr = arr(i-1:i+1, j-1:j+1), and
    ! 1/(dx^2) and 1/(dy^2) 

    real(kind=REAL64), dimension(3, 3), intent(in) :: Neighbour_rarr
    real(kind=REAL64), intent(in) :: inv_dx_square, inv_dy_square
    real(kind=REAL64) :: x_part, y_part, deriv

    ! Finite differences for d^2f(i, j)/dx^2 approx (f(i+1, j) + f(i-1, j))/(dx^2)
    x_part = (Neighbour_rarr(1, 2) + Neighbour_rarr(3, 2) - 2*Neighbour_rarr(2, 2)) * inv_dx_square
    y_part = (Neighbour_rarr(2, 1) + Neighbour_rarr(2, 3) - 2*Neighbour_rarr(2, 2)) * inv_dy_square

    ! Sum to find total derivative
    deriv = x_part + y_part
  end function

end module FieldSolver