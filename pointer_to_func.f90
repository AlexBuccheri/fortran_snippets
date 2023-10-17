module subroutines

  abstract interface
     ! Name me better
     subroutine routine(a)
       integer, intent(in) :: a(:)
     end subroutine routine
  end interface
  
contains
  
  subroutine minus_one(a)
    integer, intent(in) :: a(:)
    write(*, *) a - 1
  end subroutine minus_one

  subroutine add_one(a)
    integer, intent(in) :: a(:)
    write(*, *) a + 1
  end subroutine add_one
  
end module subroutines

! How to assign a pointer to a function or subroutine
program pointer_to_func
  use subroutines
  implicit none
  integer :: a(5) = [1, 2, 3, 4, 5]
  logical :: add = .false.
  procedure(routine), pointer :: selected_routine

  if (add) then
     selected_routine => add_one
  else
     selected_routine => minus_one
  endif

  call selected_routine(a)
  
end program pointer_to_func
