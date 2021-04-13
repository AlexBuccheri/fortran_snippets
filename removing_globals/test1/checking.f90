! gfortran global_namespace.f90 routines.f90 checking.f90 -o check
!
! So this wrapping works, and will allow me to:
!  * add an API for a routine
!  * Make it testable
!  * Make it usable elsehere

program checking
  use routines, only: initialise_a, modify_a, print_a
  implicit none

  call initialise_a()
  call modify_a()
  call print_a()

end program checking
