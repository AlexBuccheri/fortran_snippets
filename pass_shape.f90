! Examples of routines accepting an array of any shape via
! assumed-size and assumed-rank declarations, and passing them to a 1D array

program pass_shape
  use, intrinsic :: iso_fortran_env,  wp => REAL64
  implicit none
  real(wp) :: a = 1._wp
  real(wp), allocatable :: b(:), c(:,:)

  ! Some dummy data 
  allocate(b(3), c(2,2))
  b = 2._wp
  c = 3._wp

  ! Cannot pass a scalar
  ! call print_me_assumed_size(a, size(a))
  
  write(*,*) 'Pass assumed size with rank 1 and 2 arrays:'
  call print_me_assumed_size(b, size(b))
  call print_me_assumed_size(c, size(c))

  write(*,*) 'Sequence association' 
  call print_me_assumed_size_SA(c, size(c))
  write(*,*) 

  write(*,*) 'Pass assumed rank with rank 0, 1 and 2 arrays:'
  ! Can also pass a scalar with this method
  call print_me_assumed_rank(a)
  call print_me_assumed_rank(b)
  call print_me_assumed_rank(c)

  write(*,*) 'Pass assumed rank with rank 0, 1 and 2 arrays, to select rank:'
  call print_me_select_rank(a)
  call print_me_select_rank(b)
  call print_me_select_rank(c)
  
contains

  !> Assumed-size. Pass size
  subroutine print_me_assumed_size(array, n)
    real(wp),  intent (in) :: array(*)
    integer,   intent(in) :: n

    ! Can't get the shape from this though. Both size and shape are illegal.  
    !write(*,*) size(array), shape(array)
    write(*,*) array(1:n) 

  end subroutine print_me_assumed_size

  ! Also assumed-size
  ! References:
  ! https://michaelgoerz.net/notes/advanced-array-passing-in-fortran.html
  ! https://stackoverflow.com/questions/39749056/array-of-unknown-rank-as-subroutine-argument
  ! Works via sequence association
  ! https://software.intel.com/content/www/us/en/develop/blogs/doctor-fortran-in-ive-come-here-for-an-argument.html
  subroutine print_me_assumed_size_SA(array, n)
    real(wp),  intent (in) :: array(1)
    integer,   intent(in) :: n

    ! size is legal but will return 1
    write(*,*) size(array)
    write(*,*) array(1:n) 
    
  end subroutine print_me_assumed_size_SA

  
  !> Reference
  ! https://stackoverflow.com/questions/39749056/array-of-unknown-rank-as-subroutine-argument
  !
  ! Cannot use the more up-to-date syntax,
  !   array(1:size(input_array)) => input_array
  ! as "an assumed-rank variable may only be used as an actual argument"
  ! which I interpret to mean if input_array is not used in an argument
  ! to a routine or function call, it's illegal
  ! 
  subroutine print_me_assumed_rank(input_array)
    use, intrinsic :: ISO_C_BINDING
    real(wp), target, contiguous, intent(in) :: input_array(..)  !Assumed-rank 
    real(wp), pointer :: array(:) !This could be any shape

    ! size is both legal and correct in this context
    call C_F_POINTER (C_LOC(input_array), array, [size(input_array)])
    write(*,*) 'Shape of input array', shape(input_array)
    write(*,*) 'Shape of assigned array', shape(array)
    write(*,*) 'Contents of assigned array', array
    
  end subroutine print_me_assumed_rank

  
  !> F2015 and above. 
  subroutine print_me_select_rank(input_array)
    real(wp), intent(in) :: input_array(..)  ! Assumed-rank
    integer :: i

    write(*,*) 'Shape of input', shape(input_array)

    ! Deal with arrays of different shapes differently 
    select rank(input_array)
      rank (0)
        write(*,*) 'Contents of scalar', input_array
      rank (1)
        write(*,*) 'Contents of vector', input_array(:)
      rank (2)
        write(*,*) 'Contents of 2D matrix'  
        do i = 1, size(input_array, 1)
           write(*,*) input_array(i, :)
        enddo
      rank default
        write(*,*) 'Unexpected rank'
        stop
    end select
     
  end subroutine print_me_select_rank

  
end program pass_shape
