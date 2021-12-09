! Module that uses Gauss-Seidel method to find the Electric field
! Given a charge density field rho



module FieldSolver
  use ISO_FORTRAN_ENV
  use GlobalUtils
  implicit none

  contains

  ! ################
  ! # GAUSS-SEIDEL #
  ! ################

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
        deriv = Total_Deriv_2D(phi(i-1:i+1, j-1:j+1), inv_dx_square, inv_dy_square)
        phi(i, j) = inv_denom * (deriv - rho(i, j))
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

    d_rms = sqrt(sum(deriv)/N)
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
    x_part = (Neighbour_rarr(1, 2) + Neighbour_rarr(3, 2)) * inv_dx_square
    y_part = (Neighbour_rarr(2, 1) + Neighbour_rarr(2, 3)) * inv_dy_square

    ! Sum to find total derivative
    deriv = x_part + y_part
  end function

end module FieldSolver
