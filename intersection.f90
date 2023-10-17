! https://stackoverflow.com/questions/28637105/better-way-to-mask-a-fortran-array

program intersection
 
  implicit none
  real(8), allocatable :: a(:), b(:)
  logical, allocatable :: mask(:)
  real(8), allocatable :: c(:)
  integer, allocatable :: t_indices(:)
  real(8) :: tol = 0.2

  integer :: i
  
  a = [0., 0.1, 2., 3., 4.]
  b = [0., 0., 0., 0., 5.]
  
  ! Intersection of 2 arrays
  mask = (a > tol) .and. (b > tol)
  write(*,*) mask

  t_indices = pack([(i, i=1,size(mask))], mask .eqv. .true.)
  write(*,*) t_indices
  
end program intersection
