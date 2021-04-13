module global_namespace
  implicit none
  integer, allocatable :: a(:)


contains

  subroutine set_in_global_namespace(dummy_a)
    integer, allocatable, intent(inout) ::dummy_a(:)
    allocate(a(size(dummy_a)), source=dummy_a)
    deallocate(dummy_a)
  end subroutine set_in_global_namespace
  
end module global_namespace
  
  
