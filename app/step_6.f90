program step_6
   use stdlib_kinds, only: dp, i32 => int32
   use stdlib_string_type
   use stdlib_math, only: linspace, arange
   use string_conv, only: num2str
   use fortplot

   implicit none

   ! Parameters
   integer(i32), parameter :: nx = 81, ny = 81, nt = 100
   real(dp), parameter :: x_start = 0.0_dp, x_end = 2.0_dp, y_start = 0.0_dp, y_end = 2.0_dp
   real(dp), parameter :: dx = (x_end - x_start) / real(nx - 1, dp)
   real(dp), parameter :: dy = (y_end - y_start) / real(ny - 1, dp)

   real(dp), parameter :: sigma = 0.2_dp, dt = sigma * dx
   real(dp), parameter :: c = 1.0_dp
   real(dp) :: x(nx), y(ny)
   real(dp) :: u(ny, nx), un(ny, nx)
   real(dp) :: arr(6)
   ! Loop variables
   integer(i32) :: i, j, n, arr_size(2)

   arr = arange(0.0_dp, 5.0_dp, 1.0_dp)
   print *, arr

   ! Loop over the array and compute the difference between consecutive elements
   do i = 2, size(arr)
      write(*, '(A, F10.5)') "Difference: ", arr(i) - arr(i-1)
   end do

   ! That loop runs fast but we can also write it as

   print *, "Differences using array operations:"
   print *,  arr(2:) - arr(1:size(arr)-1)

   ! ----------------- Now back to solving equations -----------------
   ! Assign initial conditions
   u = 1.0_dp
   u(int(0.5_dp / dy) : int(1 / dy + 1), int(0.5_dp / dx) : int(1 / dx + 1)) = 2.0_dp
   
   
!    arr_size =  shape(u, kind=i32)

!    print *, arr_size(1), arr_size(2)

   do n = 1, nt
      un = u
      ! Don't include the boundaries in the stencil update
      do j = 2, ny - 1
         do i = 2, nx - 1
            u(j, i) = un(j, i) - c * dt/dx * (un(j, i) - un(j, i-1)) &
               - c * dt/dy * (un(j, i) - un(j-1, i))
         end do
      end do

      ! Enforce boundary conditions (u=1 at boundaries)
      u(1, :)  = 1.0_dp
      u(ny, :) = 1.0_dp
      u(:, 1)  = 1.0_dp
      u(:, nx) = 1.0_dp
   end do


   ! Now doing the same as above but using array operations
   u = 1.0_dp
   u(int(0.5_dp / dy) : int(1 / dy + 1), int(0.5_dp / dx) : int(1 / dx + 1)) = 2.0_dp
    do n = 1, nt
        un = u
    
        ! Update interior points using array operations
        u(2:ny-1, 2:nx-1) = un(2:ny-1, 2:nx-1) - c * dt/dx * (un(2:ny-1, 2:nx-1) - un(2:ny-1, 1:nx-2)) &
            - c * dt/dy * (un(2:ny-1, 2:nx-1) - un(1:ny-2, 2:nx-1))
    
        ! Enforce boundary conditions (u=1 at boundaries)
        u(1, :)  = 1.0_dp
        u(ny, :) = 1.0_dp
        u(:, 1)  = 1.0_dp
        u(:, nx) = 1.0_dp
    end do

end program step_6
