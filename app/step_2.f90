program step_2
   use stdlib_math, only: linspace
   use stdlib_kinds, only: dp, i32 => int32
   use fortplot

   implicit none

   ! Non-linear convection
   real(dp), parameter :: start = 0.0_dp, end = 2.0_dp, length = end - start
   integer(i32), parameter :: nx = 41_i32, nt = 25_i32
   real(dp), parameter :: dx = length/(nx-1), dt = 0.025_dp, c = 1.0_dp
   real(dp) :: u(nx), x(nx), un(nx)

   ! Local variables
   integer(i32) :: i, n

   u = 1.0_dp
   x = linspace(start, end, nx)
   ! Initial condition: u = 2 in the interval 0.5 <= x <= 1, u = 1 elsewhere
   u( int(0.5_dp/dx, i32) + 1 : int(1.0_dp/dx, i32) + 1 ) = 2.0_dp

   ! Plot the initial condition
   call figure()
   call plot(x, u, label= 'Initial Condition u(x,0)', linestyle=LINESTYLE_SOLID)
   call title('Non-Linear Convection')
   call xlabel('x')
   call ylabel('Velocity u(x)')

   un = 0.0_dp  ! Init a temporary array to hold the previous time step's values
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
   ! call xlim(0.0_dp,1.0_dp)
   call legend()
   call show()
end program step_2
