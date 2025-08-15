program step_2
    use kinds_module, only: wp, i32
    use stdlib_math, only: linspace
    use fortplot

    implicit none
    
    ! Non-linear convection
    real(wp), parameter :: start = 0.0_wp, end = 2.0_wp, length = end - start
    integer(i32), parameter :: nx = 41_i32, nt = 25_i32
    real(wp), parameter :: dx = length/(nx-1), dt = 0.025_wp, c = 1.0_wp
    real(wp) :: u(nx), x(nx), un(nx)

    ! Local variables
    integer(i32) :: i, n

    u = 1.0_wp
    x = linspace(start, end, nx)
    ! Initial condition: u = 2 in the interval 0.5 <= x <= 1, u = 1 elsewhere
    u( int(0.5_wp/dx, i32) + 1 : int(1.0_wp/dx, i32) + 1 ) = 2.0_wp
    
    ! Plot the initial condition
    call figure()
    call plot(x, u, label= 'Initial Condition u(x,0)', linestyle=LINESTYLE_SOLID)
    call title('Non-Linear Convection')
    call xlabel('x')
    call ylabel('Velocity u(x)')

    un = 0.0_wp  ! Init a temporary array to hold the previous time step's values
    do n = 1, nt
        un = u  ! Copy the current state of u into un
        do i = 2, nx
            u(i) = un(i) - un(i) * dt/dx * (un(i) - un(i-1))
        end do
    end do
    
    ! Plotting the results
    call plot(x, u, label='Final State u(x,nt)', linestyle=LINESTYLE_DASHED)
    call title('Non-Linear Convection: Final Velocity Profile')
    call xlabel('x')
    call ylabel('Velocity u(x)')
    ! call xlim(0.0_wp,1.0_wp)
    call legend()
    call show()
end program step_2