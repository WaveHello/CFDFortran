program step_8
    use stdlib_kinds, only: dp, i32 => int32
    use stdlib_string_type
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
    real(dp) :: u(ny, nx) = 1.0_dp, un(ny, nx) = 1.0_dp, u_init(ny, nx) = 1.0_dp
    real(dp) :: v(ny, nx) = 1.0_dp, vn(ny, nx) = 1.0_dp, v_init(ny, nx) = 1.0_dp
    real(dp) :: x(nx) = 0.0_dp, y(ny) = 0.0_dp

    ! Loop variables
    integer(i32) :: i, j, n

    ! Create the linear spaces for meshgrid
    x = linspace(x_start, x_end, nx)
    y = linspace(y_start, y_end, ny)

    ! Assign the initial conditions
    u(int(0.5_dp / dy) : int(1 / dy + 1), int(0.5_dp / dx) : int(1 / dx + 1)) = 2.0_dp
    v(int(0.5_dp / dy) : int(1 / dy + 1), int(0.5_dp / dx) : int(1 / dx + 1)) = 2.0_dp
    
    ! Store initial conditions for plotting later
    u_init = u
    v_init = v  

    do n = 1, nt
       un = u
       vn = v

       u(2:ny-1, 2:nx-1) = un(2:ny-1, 2:nx-1) - &
          c * dt/dx * (un(2:ny-1, 2:nx-1) - un(2:ny-1, 1:nx-2)) - &
          c * dt/dy * (un(2:ny-1, 2:nx-1) - un(1:ny-2, 2:nx-1))

       v(2:ny-1, 2:nx-1) = vn(2:ny-1, 2:nx-1) - &
          c * dt/dx * (vn(2:ny-1, 2:nx-1) - vn(2:ny-1, 1:nx-2)) - &
          c * dt/dy * (vn(2:ny-1, 2:nx-1) - vn(1:ny-2, 2:nx-1))

       ! Enforce boundary conditions (u=1 at boundaries)
       u(1, :)  = 1.0_dp
       u(ny, :) = 1.0_dp
       u(:, 1)  = 1.0_dp
       u(:, nx) = 1.0_dp

       v(1, :)  = 1.0_dp
       v(ny, :) = 1.0_dp
       v(:, 1)  = 1.0_dp
       v(:, nx) = 1.0_dp
    end do

    ! Plot the final results
    call figure()
    call contour_filled(x, y, u_init, label = "Initial Condition u", colormap='viridis')
    call contour_filled(x, y, u, label = "Final Condition u @ t= " // num2str(nt * dt) , colormap='viridis')
    call title('2D Linear Convection Equation - u component')
    call xlabel('X-axis')
    call ylabel('Y-axis')
    call show()

    call figure()
    call contour_filled(x, y, v_init, label = "Initial Condition v", colormap='viridis')
    call contour_filled(x, y, v, label = "Final Condition v @ t= " // num2str(nt * dt) , colormap='viridis')
    call title('2D Linear Convection Equation - v component')
    call xlabel('X-axis')
    call ylabel('Y-axis')
    call show()

end program step_8