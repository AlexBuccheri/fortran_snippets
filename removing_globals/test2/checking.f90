! gfortran global_namespace.f90 routines.f90 checking.f90 -o check
!
! So this wrapping works, and will allow me to:
!  * add an API for a routine
!  * Make it testable
!  * Make it usable elsewhere

program checking
  use routines, only: initialise_a, modify_a, print_a, modify_a_implementation
  use global_namespace, only: set_in_global_namespace
  implicit none

  integer, allocatable :: a(:)

  allocate(a(3), source=5)
  call modify_a_implementation(a, 6)
  write(*,*) 'local a:', a  
  call print_a()
  call set_in_global_namespace(a)
  call print_a()

end program checking
