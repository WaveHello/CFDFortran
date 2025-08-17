program step_3
   use stdlib_math, only: linspace
   use stdlib_kinds, only: dp, i32 => int32
   use fortplot

   implicit none

   call figure()
   ! Call with 41 grid points
   call linearconv(41_i32)
   call linearconv(81_i32)
   call linearconv(61_i32)
   call linearconv(71_i32)
   ! Starts generating spurios solutions at about this many grid points
   ! This is because of the CFL condition
   ! TODO: Add more information about the CFL condition here
   call linearconv(85_i32)

   ! Format the plot
   call title('Non-Linear Convection: Comparison of Grid Resolutions')
   call xlabel('x')
   call ylabel('Velocity u(x)')
   call xlim(0.0_dp, 2.0_dp)
   call legend(location = 'upper left')
   call show(blocking = .true.)

contains
   subroutine linearconv(nx)
      integer(i32), intent(in) :: nx
      real(dp), parameter :: start = 0.0_dp, end = 2.0_dp, length = end - start
      integer(i32), parameter :: nt = 25_i32
      real(dp), parameter :: c = 1.0_dp, dt = 0.025_dp
      real(dp), allocatable :: dx
      real(dp) :: un(nx)
      real(dp) :: uout(nx), x(nx)
      character(len=20) :: label_str

      ! Local variables
      integer(i32) :: i, n

      ! Calc the grid spacing
      dx = length/(real(nx, dp) -1)

      x = linspace(start, end, nx)

      ! Initial condition: u = 2 in the interval 0.5 <= x <= 1, u = 1 elsewhere
      uout = 1.0_dp
      uout( int(0.5_dp/dx, i32) + 1 : int(1.0_dp/dx, i32) + 1 ) = 2.0_dp

      un = 0.0_dp
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
