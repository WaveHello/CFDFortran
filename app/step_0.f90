program step_0
    use kinds_module, only: wp, i32
    use stdlib_math, only: linspace
implicit none

real(wp), allocatable :: myarray(:), myvals(:)
integer(i32) :: a, i, j
real(wp) :: c
character(len=:), allocatable :: b

! Creating a linearly spaced array from 0.0 to 5.0 with 10 points
myarray = linspace(0.0_wp, 5.0_wp, 10)

! Printing the array
print *, myarray

a = 5_i32! a is an integer 5
b = "five" ! b is the string "five"
c = 5.0_wp ! c is a real 5.0

do i = 1, 5
    print *, "Hi there! This is loop iteration number ", i
end do 

do i = 1, 3
    do j = 1, 3
        print*, "i =", i, " j =", j
    end do
    print *, "This state is within the i-loop, but not in the j-loop"
end do


! In Fortran, you can look at portions of arrays in the same way as in MatLab, with a few extra tricks thrown in. Let's take an array of values from 1 to 5.

myvals = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]

! Standard indexing starts at 1, so myvals(1) is 1.0, myvals(2) is 2.0, etc.
print *, "Accessing individual elements:"
print *, myvals(1), myvals(5)

! There are 5 elements in the array myvals, but if we try to look at myvals(6), we will get an error.

! This prints the first 3 elements of myvals: 1.0, 2.0, 3.0
print *, "Accessing a range of elements:"
print *, myvals(1:3)

end program step_0
