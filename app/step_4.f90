program step_4
   use stdlib_math, only:linspace
   use stdlib_kinds, only: dp, i32 => int32
   use fortplot

   implicit none

   ! Diffusion equation
   integer(i32), parameter :: nx = 41, nt = 20
   real(dp), parameter :: dx = 2.0_dp / (real(nx, dp) - 1), nu = 0.3_dp, &
      sigma = 0.3_dp, dt = sigma * dx**2 / nu, &
      start = 0.0_dp, end = 2.0_dp, length = end - start

   real(dp) :: u(nx), un(nx), x(nx)
   ! Counters
   integer(i32) :: i, n

   x = linspace(start, end, nx)

   ! Setting u = 2 between 0.5 and 1 as
   u = 1.0_dp
   u(int(0.5_dp/dx):int(1.0_dp/dx+1)) = 2.0_dp

   do n = 1, nt
      un = u
      do i = 2, nx-1
         u(i) = un(i) + nu * dt / dx**2 * (un(i+1) - 2*un(i) + un(i-1))
      end do
   end do

   call figure()
   call plot(x, u, label = "Final Condition")
   call xlabel("x")
   call ylabel("Velocity")
   call legend()
   call show()

end program step_4
