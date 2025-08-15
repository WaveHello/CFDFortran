program step_3
    use kinds_module, only: wp, i32
    use stdlib_math, only: linspace
    use fortplot

    implicit none
    
    call figure()
    ! Call with 41 grid points
    call linearconv(41_i32)
   
    call linearconv(81_i32)
    call linearconv(61_i32)
    call linearconv(71_i32)
    call linearconv(85_i32)

    call title('Non-Linear Convection: Comparison of Grid Resolutions')
    call xlabel('x')
    call ylabel('Velocity u(x)')
    call xlim(0.0_wp, 2.0_wp)
    call legend(location = 'upper left')
    call show()

    contains
    subroutine linearconv(nx)
        integer(i32), intent(in) :: nx
        real(wp), parameter :: start = 0.0_wp, end = 2.0_wp, length = end - start
        integer(i32), parameter :: nt = 25_i32
        real(wp), parameter :: c = 1.0_wp, dt = 0.025_wp
        real(wp), allocatable :: dx
        real(wp) :: un(nx)
        real(wp) :: uout(nx), x(nx)
        character(len=20) :: label_str

        ! Local variables
        integer(i32) :: i, n

        ! Calc the grid spacing
        dx = length/(real(nx, wp) -1)

        x = linspace(start, end, nx)
        
         ! Initial condition: u = 2 in the interval 0.5 <= x <= 1, u = 1 elsewhere
        uout = 1.0_wp
        uout( int(0.5_wp/dx, i32) + 1 : int(1.0_wp/dx, i32) + 1 ) = 2.0_wp

        un = 0.0_wp
        do n = 1, nt
            un = uout
            do i = 2, nx
                uout(i) = un(i) - c * dt/dx * (un(i) - un(i-1))
            end do
        end do

        write(label_str, '(I10)') nx

        ! Label the plot with the number of grid points used
        call plot(x, uout, label = 'nx= '//adjustl(label_str), linestyle=LINESTYLE_SOLID)        
    end subroutine linearconv

    
end program step_3