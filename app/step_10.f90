program step_10
    use stdlib_math, only: linspace
    use stdlib_kinds, only: dp, i32 => int32
    use string_conv, only: num2str
    use fortplot

    implicit none
    
    ! We're going to write some more functions and subroutines in this modules.
    ! Functions in fortran return a single value, whereas subroutines can return multiple values via intent(out) arguments.
    ! Inputs and outputs can be specified with intent(in), intent(out) and intent(inout).
    ! Functions are referenced by their name, subroutines are called with the 'call' keyword.
    ! Both are held in the contains section of the program or module.
    ! Fortran is strongly typed, so we need to declare the types of arguments and return values.
    ! The function or subroutine will not work for other types unless we use generics or interfaces, which we won't cover here.

    ! We also need to initialize variables before using them.
    integer(i32) :: n

    print *, "Simple function example:", simple_add(3.0_dp, 4.0_dp)

    print *, "Fibonacci sequence:", fibonacci(8_i32)

    ! Once defined, functions can be used like any other expression
    do n = 0, 9
        print *, "Fibonacci(" // num2str(n) // ") = ", fibonacci(n)
    end do

    ! We'll use the capacity to define functions to help build code that is easier to resuse, easier to maintain, and easier to share!
    
    contains

    function simple_add(a, b) result(retval)
        real(dp), intent(in) :: a, b
        real(dp) :: retval
        
        ! The name of the variable in the result() clause is the return value of the function
        retval = a + b
    end function simple_add

    function fibonacci(n) result(fib_n)
        integer(i32), intent(in) :: n
        integer(i32) :: fib_n

        ! Local variables
        ! Don't set the value of variables in the define section
        ! Everytime the function is called these variables carry over there value from the last call
        integer(i32) :: i, temp, a, b

        a = 0
        b = 1
        do i = 1, n
            temp = a + b
            a = b
            b = temp
        end do

        fib_n = a

    end function fibonacci


end program step_10