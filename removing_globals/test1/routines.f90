module routines
  implicit none

contains

  subroutine initialise_a()
    use global_namespace, only: a
    allocate(a(3), source=1)
  end subroutine initialise_a

  subroutine modify_a()
    use global_namespace, only: a
    call modify_a_implementation(a, 2)
  end subroutine modify_a

  ! This implementation was originally in modify_a, however I've abstracted
  ! it to a new routine, such that I can write an API without breaking the
  ! calls to modify_a() elsewhere in the code 
  subroutine modify_a_implementation(a, value_a)
    implicit none
    integer, intent(inout) :: a(:)
    integer, intent(in) :: value_a
    a = value_a
  end subroutine modify_a_implementation

  subroutine print_a()
    use global_namespace, only: a
    write(*,*) 'a:', a
  end subroutine print_a
  
end module routines
