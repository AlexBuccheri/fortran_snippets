! Outer product with dger
program dger_example
  implicit none
  integer, parameter :: m = 3, n = 2
  integer :: i, j
  double precision :: alpha
  double precision :: A(m, n), x(m), y(n)
  double precision :: B(m*n)
  
  alpha = 1.0d0
  
  x = [ 1.0d0, 2.0d0, 3.0d0]
  y = [ 1.0d0, -1.0d0]
  A = 0.d0
  
  ! Call DGER to perform the rank-1 update
  call dger(m, n, alpha, x, 1, y, 1, A, m)
  
  ! Print updated matrix A
  print *, 'Updated matrix A after DGER:'
  do i = 1, m
    print *, (A(i, j), j = 1, n)
  end do


  call dger(n, m, alpha, y, 1, x, 1, B, n)
  
  ! Print updated matrix A
  print *, 'Updated matrix B after DGER:'
  do i = 1, m*n
    print *, B(i)
 end do


end program dger_example
