program step_5
   use stdlib_math, only: linspace
   use stdlib_constants, only: pi_dp
   use stdlib_kinds, only: dp, i32 => int32, sp
   use stdlib_string_type
   use string_conv, only: num2str
   use fortplot

   implicit none

   ! Solving burgers equation

   real(dp), parameter :: start = 0.0_dp, end = 2.0_dp * pi_dp, length = end - start
   integer(i32), parameter :: nx = 101_i32, nt = 100_i32

   ! For periodic BCs, x_0 and x_nx are the same point
   ! so we only need nx points, not nx + 1
   real(dp), parameter :: dx = length / real(nx, dp)
   real(dp), parameter :: nu = 0.07_dp
   real(dp), parameter :: dt = dx * nu
   real(dp) :: t = 0.0_dp
   real(dp) :: u(nx), un(nx), x(nx)

   integer(i32) :: i, n

   ! Test the initial condition function
   print *, u_analytical(1.0_dp, 4.0_dp, 3.0_dp)


   x = linspace(start, end, nx)
   u = u_analytical(t, x, nu)


   ! Plot the initial condition
   call figure()
   ! call plot(x, u, label='t = 0.0 s', linestyle='--')

   ! Time-stepping loop
   do n = 1, nt
      un = u
      do i = 2, nx - 1_i32
         u(i) = un(i) - un(i) * dt/dx * (un(i) - un(i-1)) + nu * dt/dx**2 * &
            (un(i+1) - 2.0_dp * un(i) + un(i-1))
      end do

      ! Periodic BCs - setting the first and last elements
      ! Apply the stencil to the first point
      u(1) = un(1) - un(1) * dt/dx * (un(1) - un(nx-1_i32)) + nu * dt/dx**2 * &
         (un(2) - 2.0_dp * un(1) + un(nx-1_i32))

      ! In periodic BCs, the last point literally has the same as the first point
      u(nx) = u(1)
   end do

   ! Plot the final result
   print *, num2str(nx)

   call plot(x, u, label='Computed u @ t = ' // num2str(nt * dt), linestyle=LINESTYLE_DASHDOT)
   call plot(x, u_analytical(nt*dt, x, nu), label='Analytical Soln @ t = ' // num2str(nt * dt))
   ! Format the plot
   call title('1D Viscous Burgers Equation: Numerical Solution vs Analytical Solution')
   call xlabel('x')
   call ylabel('Velocity u(x,t)')
   ! call xlim(start, 6.5_dp)
   call ylim(0.0_dp, 10.0_dp)
   call legend(location = 'upper right')
   call show()
contains


   
   ! Making the functions elemental tells the compiler to determine which elements are arrays and which are scalars
   elemental pure function u_analytical(t, x, nu) result(retval)
      real(dp), intent(in) :: t, x, nu
      real(dp) :: retval

      retval = -2.0_dp * nu * ( phi_prime(t, x, nu) / phi(t, x, nu) ) + 4.0_dp
   end function u_analytical

   elemental pure function phi(t, x, nu) result(retval)
      real(dp), intent(in) :: t, x, nu
      real(dp) :: retval

      ! Local variables
      real(dp) :: first_super, second_super

      first_super  = (- 4.0_dp * t + x - 2.0_dp * pi_dp )**2 / (4.0_dp * nu * (t + 1.0_dp) )
      second_super = (- 4.0_dp * t + x                  )**2 / (4.0_dp * nu * (t + 1.0_dp) )
      retval = exp(- first_super) + exp(- second_super)
   end function phi

   elemental pure function phi_prime(t, x, nu) result(retval)
      real(dp), intent(in) :: t, x, nu
      real(dp) :: retval

      real(dp) :: first_super_term, second_super_term
      real(dp) :: exponential_term_1, exponential_term_2
      real(dp) :: denominator

      ! Calculate common denominator
      denominator = 4.0_dp * nu * (t + 1.0_dp)

      ! Calculate terms inside the exponentials
      first_super_term  = -4.0_dp * t + x - 2.0_dp * pi_dp
      second_super_term = -4.0_dp * t + x

      ! Calculate the exponential terms
      exponential_term_1 = exp(-(first_super_term**2) / denominator)
      exponential_term_2 = exp(-(second_super_term**2) / denominator)

      ! Calculate the final result
      retval = -(-8.0_dp*t + 2.0_dp*x - 4.0_dp*pi_dp) * exponential_term_1 / denominator &
         - (-8.0_dp*t + 2.0_dp*x) * exponential_term_2 / denominator
   end function phi_prime

end program step_5
