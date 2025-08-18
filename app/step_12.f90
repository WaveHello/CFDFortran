program step_12
    use stdlib_kinds, only: dp, i32 => int32
    use stdlib_math, only: linspace
    use stdlib_linalg, only: norm
    use string_conv, only: num2str
    use fortplot

    implicit none
    
    ! Parameters for 2d laplace equation
    integer(i32), parameter :: nx = 31, ny = 31, nt = 500
    real(dp), parameter :: x_start = 0.0_dp, x_end = 2.0_dp, y_start = 0.0_dp, y_end = 2.0_dp
    real(dp), parameter :: dx = (x_end - x_start) / real(nx - 1, dp)
    real(dp), parameter :: dy = (y_end - y_start) / real(ny - 1, dp)
    real(dp) :: p(ny, nx)= 0.0_dp
    real(dp) :: x(nx) = 0.0_dp, y(ny) = 0.0_dp

    ! Loop variables
    integer(i32) :: i, j, n

    ! Set initial conditions
    x = linspace(x_start, x_end, nx)
    y = linspace(y_start, y_end, ny)

    ! Set the boundary conditions
    p(:, 1) = 0.0_dp          ! p = 0 @ x = 0
    p(1:, nx) = y              ! p = y @ x = 2
    p(1, :) = p(2, :)         ! dp/dy = 0 @ y = 0
    p(ny, :) = p(ny - 1, :)   ! dp/dy = 0 @ y = 2

    ! Plot the initial conditions
    call plot2d(x, y, p)

    ! Solve the laplace equation using the laplace2d function
    p = laplace2d(p, y, dx, dy, 1.0e-6_dp)

    ! Plot the final results
    call plot2d(x, y, p)

    contains

    subroutine plot2d(x, y, p)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in) :: p(:, :)
        call figure()
        call contour_filled(x, y, p)
        call xlabel('X-axis')
        call ylabel('Y-axis')
        call show()
    end subroutine plot2d

    ! If a function doesn't modify any variables outside of it scope and any input variables
    ! are not modified, it can be marked as pure. This allows the compiler to make optimizations
    ! and also allows the function to be used in certain contexts (like array expressions).
    pure function laplace2d(p, y, dx, dy, l1norm_target) result(p_new)
        real(dp), intent(in) :: p(:, :)
        real(dp), intent(in) :: y(:), l1norm_target
        real(dp), intent(in) :: dx, dy
        real(dp) :: p_new(size(p, 1), size(p, 2)), p_old(size(p, 1), size(p, 2))
        integer(i32) :: i, j
        real(dp) :: l1norm

        ! Set lnorm to a huge number initially
        l1norm = huge(1.0_dp)

        ! Store the value of p in another variable p_new so p isn't updated and can remain intent(in)
        p_new = p
        do while (l1norm > l1norm_target)
            p_old = p_new
            p_new(2:ny-1, 2:nx-1) = (dy**2 * (p_old(2:ny-1, 3:nx) + p_old(2:ny-1, 1:nx-2) ) + &
                                     dx**2 * (p_old(3:ny, 2:nx-1) + p_old(1:ny-2, 2:nx-1) )) / &
                                    (2.0_dp * (dx**2 + dy**2))

            ! Enforce boundary conditions
            p_new(:, 1) = 0.0_dp
            p_new(:, nx) = y
            p_new(1, :) = p(2, :) ! dp/dy = 0 @ y = 0
            p_new(ny, :) = p(ny - 1, :) ! dp/dy = 0 @ y = 2

            ! Compute the L1 norm to check for convergence
            l1norm = sum(abs(p_new - p_old)) / sum(abs(p_new))

        end do

    end function laplace2d
end program step_12