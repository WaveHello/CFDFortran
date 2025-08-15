program step_1
    use stdlib_math, only: linspace
    use kinds_module, only: wp, i32
    use fortplot


    implicit none
    
    real(wp), parameter :: start = 0, end = 2.0_wp, length = end - start
    integer(i32), parameter :: nx = 41_i32, nt = 25_i32
    real(wp), parameter :: dx = length/(nx-1), dt = 0.025_wp, c = 1.0_wp

    ! Local variables
    integer(i32) :: i, n
    
    real(wp) :: u(nx), x(nx), un(nx)

    ! Create our spatial grid from 0 to 2.0 with nx points
    x = linspace(start, end, nx)

    ! Set all the values of u to 1.0 initially
    u(:) = 1.0_wp

    ! We also need to set up our initial conditions. The initial velocity is given as u = 2 in the interval 0.5 <= x <= 1, and u = 1 elsewhere.
    u( int(0.5_wp/dx, i32) + 1 : int(1.0_wp/dx, i32) + 1 ) = 2.0_wp
    
    print *, "Initial condition:"
    print *, u

    ! Now let's take a look at those initial conditions using a plot. We've imported the pyplot module, which provides a Fortran interface to the popular Python plotting library Matplotlib.

    call figure()
    call plot(x, u)
    call title("Initial Condition: Velocity u(x) at t=0")
    call xlabel("x")
    call ylabel("Initial Velocity u(x)")
    call legend("u(x,0)")
    ! call grid()

    ! For every element of our array u we need ot perform the operation u_i^{n+1} = u_^{n} - c dt/dx (u_i^{n} - u_{i-1}^{n})

    un = 0.0_wp  ! Init a temporary array to hold the previous time step's values
    do n = 1, nt
        un = u  ! Copy the current state of u into un
        do i = 2, nx
            u(i) = un(i) - c * dt/dx * (un(i) - un(i-1))
        end do
    end do

    ! Add the final result and plot it
    call plot(x, u, linestyle=LINESTYLE_DASHED) ! Don't think
    call legend("u(x,nt)")
    call show(.false.)
end program step_1