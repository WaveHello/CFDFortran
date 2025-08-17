program step_9
    use stdlib_kinds, only: dp, i32 => int32
    use stdlib_string_type
    use stdlib_math, only: linspace, meshgrid
    use string_conv, only: num2str
    use fortplot

    implicit none
    
    ! 2-d linear diffusion equation parameters
    integer(i32), parameter :: nx = 31, ny = 31, nt = 17
    real(dp), parameter :: x_start = 0.0_dp, x_end = 2.0_dp, y_start = 0.0_dp, y_end = 2.0_dp
    real(dp), parameter :: dx = (x_end - x_start) / real(nx - 1, dp)
    real(dp), parameter :: dy = (y_end - y_start) / real(ny - 1, dp)
    real(dp), parameter :: nu = 0.05_dp
    real(dp), parameter :: sigma = 0.25_dp
    real(dp), parameter :: dt = sigma * dx * dy / nu
    real(dp) :: u(ny, nx) = 1.0_dp, un(ny, nx) = 1.0_dp, u_init(ny, nx) = 1.0_dp
    real(dp) :: x(nx) = 0.0_dp, y(ny) = 0.0_dp

    ! Loop variables
    integer(i32) :: i, j, n

    ! Create the linear spaces for meshgrid
    x = linspace(x_start, x_end, nx)
    y = linspace(y_start, y_end, ny)

    ! Assign the initial conditions
    u(int(0.5_dp / dy) : int(1 / dy + 1), int(0.5_dp / dx) : int(1 / dx + 1)) = 2.0_dp
    ! Store initial conditions for plotting later
    u_init = u

    call diffuse(10)

    call diffuse(14)

    call diffuse(50)

    contains
    ! Subroutine to perform time-stepping and plot the result. Take in the number of time steps
    subroutine diffuse(nt)
        integer(i32), intent(in) :: nt
        do n = 1, nt
           un = u

           u(2:ny-1, 2:nx-1) = un(2:ny-1, 2:nx-1) + &
              nu * dt/dx**2 * &
              ! un(i, j+1) - 2*un(i,j) + un(i, j-1)
              (un(2:ny-1, 3:nx) - 2.0_dp * un(2:ny-1, 2:nx-1) + un(2:ny-1, 1:nx-2)) + &
              nu * dt/dy**2 * &
              (un(3:ny, 2:nx-1) - 2.0_dp * un(2:ny-1, 2:nx-1) + un(1:ny-2, 2:nx-1))

           ! Enforce boundary conditions (u=1 at boundaries)
           u(1, :)  = 1.0_dp
           u(ny, :) = 1.0_dp
           u(:, 1)  = 1.0_dp
           u(:, nx) = 1.0_dp
        end do

        ! Plot the final results
        call figure()
        call contour_filled(x, y, u_init, label = "Initial Condition", colormap='viridis')
        call contour_filled(x, y, u, label = "Final Condition @ t= " // num2str(nt * dt) , colormap='viridis')  
        call title('2D Diffusion Equation - nt = ' // num2str(nt))
        call xlabel('X-axis')
        call ylabel('Y-axis')
        call show()
    
    end subroutine diffuse
end program step_9