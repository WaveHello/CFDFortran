program step_7
    use stdlib_kinds, only: dp, i32 => int32
    use stdlib_math, only: linspace, meshgrid
    use string_conv, only: num2str
    use fortplot

    implicit none

    ! 2-d linear convection equation parameters
    integer(i32), parameter :: nx = 81, ny = 81, nt = 100
    real(dp), parameter :: x_start = 0.0_dp, x_end = 2.0_dp, y_start = 0.0_dp, y_end = 2.0_dp
    real(dp), parameter :: dx = (x_end - x_start) / real(nx - 1, dp)
    real(dp), parameter :: dy = (y_end - y_start) / real(ny - 1, dp)
    real(dp), parameter :: sigma = 0.2_dp, dt = sigma * dx
    real(dp), parameter :: c = 1.0_dp
    real(dp) :: u(ny, nx), un(ny, nx)
    real(dp) :: x(nx), y(ny), x_grid(ny, nx), y_grid(ny, nx)

    
    ! Loop variables
    integer(i32) :: i, j, n

    ! Create the meshgrid for plotting
    x = linspace(x_start, x_end, nx)
    y = linspace(y_start, y_end, ny)
    call meshgrid(x, y, x_grid, y_grid)

    print *, shape(x_grid)
    print *, shape(y_grid)
    print *, shape(u)
    ! Set the initial condition
    u = 1.0_dp
    u(int(0.5_dp / dy) : int(1 / dy + 1), int(0.5_dp / dx) : int(1 / dx + 1)) = 2.0_dp  

    ! Plot the initial condition
    call figure()
    call contour_filled(x, y, u, label = "Initial Condition", colormap='viridis') ! Might need to change the plotting package
    ! call show()

    ! Frankly even though auto-complete is doing most of the work, I don't want to write any more loops
    ! Moving on to array operations

    do n = 1, nt
       un = u

       u(2:ny-1, 2:nx-1) = un(2:ny-1, 2:nx-1) - &
          c * dt/dx * (un(2:ny-1, 2:nx-1) - un(2:ny-1, 1:nx-2)) - &
          c * dt/dy * (un(2:ny-1, 2:nx-1) - un(1:ny-2, 2:nx-1))

       ! Enforce boundary conditions (u=1 at boundaries)
       u(1, :)  = 1.0_dp
       u(ny, :) = 1.0_dp
       u(:, 1)  = 1.0_dp
       u(:, nx) = 1.0_dp
    end do

    call contour_filled(x, y, u, label = "Final Condition @ t= " // num2str(nt * dt) , colormap='viridis')
    call title('2D Linear Convection Equation')
    call xlabel('X-axis')
    call ylabel('Y-axis')
    ! call colorbar('Velocity u')
    call show()
end program step_7