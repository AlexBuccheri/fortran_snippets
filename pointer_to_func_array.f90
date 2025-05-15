! Like an array of derived type, fortran can define
! an array using an abstract interface
! Note, this does not compile (even though I thought it did at one point)
! More disgusting solution is: https://community.intel.com/t5/Intel-Fortran-Compiler/Static-array-of-procedure-pointers/td-p/964019

program array_of_function_pointers
    abstract interface
        real function my_function(x)
            real, intent(in) :: x
        end function my_function
    end interface

    procedure(my_function), pointer :: function_ptr_array(:)

    real :: result
    integer :: i

    allocate(function_ptr_array(3))

    function_ptr_array(1) => my_square
    function_ptr_array(2) => my_cube
    function_ptr_array(3) => my_sqrt

    do i = 1, 3
        result = function_ptr_array(i)(2.0)
        print *, "Result:", result
    end do

contains

    real function my_square(x)
        real, intent(in) :: x
        my_square = x * x
    end function my_square

    real function my_cube(x)
        real, intent(in) :: x
        my_cube = x * x * x
    end function my_cube

    real function my_sqrt(x)
        real, intent(in) :: x
        my_sqrt = sqrt(x)
    end function my_sqrt

end program array_of_function_pointers
