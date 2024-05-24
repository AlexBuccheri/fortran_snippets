! ----------------------------------------------------------------------
! Example driver performing QR decomposition with pivoting
! The original input matrix is reconstructed from Q and R
!
! Input matrix based on https://uk.mathworks.com/help/matlab/ref/qr.html
! Note: I get (approximately) the same R, but not the same Q
! ----------------------------------------------------------------------
! Compile on mac:  gfortran -g -fbacktrace -fbounds-check -fcheck=all test_qr.f90 -framework Accelerate -o qr
!
! Same test in python:
!```
!  """ QR Decomposition wih pivot
!  """
!  import numpy as np
!  import scipy
!
!  m = np.array([[17., 24.,  1.,  8., 15.],
!                [23.,  5.,  7., 14., 16.],
!                [ 4.,  6., 13., 20., 22.],
!                [10., 12., 19., 21.,  3.],
!                [11., 18., 25.,  2.,  9.]])
!
!  Q, R, p = scipy.linalg.qr(m, pivoting=True)
!  rebuilt_m = np.empty_like(m)
!  # Apply pivoting indices
!  rebuilt_m[:, p] =  Q @ R
!
!  print('m', m)
!  print('R', R)
!  print('Q', Q)
!  print('Pivot column indices', p)
!  print('Rebuilt m')
!  print(rebuilt_m)
! ```
!
! Expected data (within numerical tolerance):
!
!R [[-34.71310992 -20.02125427 -23.47816148 -23.47816148 -20.02125427]
! [  0.         -25.5763441  -11.7271553  -15.4415251  -17.17013878]
! [  0.           0.         -20.40219993   3.05404464  -9.48823539]
! [  0.           0.           0.          17.49308571   3.64613345]
! [  0.           0.           0.           0.          16.00046287]]
!
!Q [[-0.02880756 -0.64212604  0.01012932  0.7647199   0.04410384]
! [-0.20165292 -0.74141384 -0.02798154 -0.63439599  0.08000231]
! [-0.37449828  0.13676409 -0.62793829  0.07071746  0.66463461]
! [-0.54734364  0.03747628 -0.42097753  0.05795162 -0.72002083]
! [-0.720189    0.13368162  0.65390049  0.06622577  0.17744103]]
!
!Pivot column indices [2 0 3 1 4]
!
!Rebuilt m
![[17. 24.  1.  8. 15.]
! [23.  5.  7. 14. 16.]
! [ 4.  6. 13. 20. 22.]
! [10. 12. 19. 21.  3.]
! [11. 18. 25.  2.  9.]]

program test_qr
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none

  integer, parameter    :: n = 5
  integer, parameter    :: m = 5
  real(dp)              :: magic_m(n, m), rebuilt_m(n, m)
  real(dp), allocatable :: A(:, :), R(:, :), Q(:, :), tmp(:, :)
  integer,  allocatable :: p(:)
  integer               :: i

  magic_m = transpose(reshape( &
       [17.,    24.,     1.,     8.,    15., &
        23.,     5.,     7.,    14.,    16., &
         4.,     6.,    13.,    20.,    22., &
        10.,    12.,    19.,    21.,     3., &
        11.,    18.,    25.,     2.,     9.], [5, 5]))

  write(*, *) 'Magic input matrix'
  do i = 1, 5
     write(*, *) magic_m(i, :)
  enddo
  write(*, *)
  
  allocate(R(n, m))
  allocate(Q(n, m))
  allocate(p(m))

  call qr_decomposition(magic_m, p, Q=Q, R=R)

  write(*, *) 'R'
  do i = 1, n
     write(*, *) R(i, :)
  enddo
  write(*, *)

  write(*, *) 'Q'
  do i = 1, n
     write(*, *) Q(i, :)
  enddo
  write(*, *)

  write(*, *) 'Pivot indices:', p

  ! Reconstruct m, applying the pivoting
  rebuilt_m(:, p) = matmul(Q, R)
  
  write(*, *) 'Rebuilt m'
  do i = 1, n
     write(*, *) rebuilt_m(i, :)
  enddo
  
contains

  !>@brief Perform QR decomposition with column-pivoting, on matrix A
  !!
  !! Utilises lapack `dgeqp3` to obtain R and p, and `dorgqr` to obtain Q.
  !! 
  !! The matrix Q is represented as a product of elementary reflectors
  !!  Q = H(1) H(2) . . . H(k), where k = min(m,n).
  !! Each H(i) has the form:
  !!  H(i) = I - tau * v * v**T
  !! where tau is a real scalar, and v is a real/complex vector
  !! with v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
  !! A(i+1:m,i), and tau in TAU(i).
  subroutine qr_decomposition_with_pivot(A, p, Q, R, preserve_A)
    real(dp), intent(inout)         :: A(:, :)     !< Input matrix to decompose as Q and R
    integer,  intent(out)           :: p(:)        !< Maps new column order to old column order, size(m)
    real(dp), intent(out), optional :: Q(:, :)     !< Orthogonal matrix 
    real(dp), intent(out), optional :: R(:, :)     !< Upper triangular matrix
    logical,  intent(in),  optional :: preserve_A  !< Preserve A, true by default. Else A is mutated
    !                                                 and contains R in the upper triangle
    integer               :: n, m, info, lwork, i, j, cnt
    real(dp), allocatable :: tau(:)                !< The scalar factors of the elementary reflectors.
    real(dp), allocatable :: work(:), tmp_A(:, :)  !< Work arrays

    n = size(A, 1)
    m = size(A, 2)

    if (present(preserve_A)) then
       keep_A = preserve_A
    else
       keep_A = .true.
    endif
    
    if (keep_A) allocate(tmp_A(n, m), source=A)
    
    allocate(tau(min(n, m)))

    ! Initialise pivot indices to zero
    p = 0
   
    ! Query the workspace
    allocate(work(1))
    lwork = -1
    call dgeqp3(n, m, A, n, p, tau, work, lwork, info)

    ! Allocate optimal work space
    lwork = int(work(1))
    deallocate(work)
    allocate(work(lwork))
    
    ! A P = Q R
    call dgeqp3(n, m, A, n, p, tau, work, lwork, info)
    deallocate(work)

    if (info /= 0) then
       write(*, *) 'Warning: dgeqp3 returned info: ', info
       error stop 101
    endif

    if (present(R)) then
       ! Extract R from upper triangle of A
       ! Note, memory access is suboptimal
       R = 0._dp
       do j = 1, m
          do i = 1, j
             R(i, j) = A(i, j)
          enddo
       enddo
    endif

    if (present(Q)) then
       ! Query optimal workspace
       allocate(work(1))
       lwork = -1
       ! min(n, m) == size(tau), the number of elementary reflectors
       call dorgqr(n, m, min(n, m), A, n, tau, work, lwork, info)
       
       ! Allocate optimal work space
       lwork = int(work(1))
       deallocate(work)
       allocate(work(lwork))

       ! Compute Q from the lower triangle of A, and tau
       call dorgqr(n, m, min(n, m), A, n, tau, work, lwork, info)

       if (info /= 0) then
          write(*, *) 'Warning: dgeqp3returned info: ', info
          error stop 102
       endif
       
       Q = A
    endif

    if (keep_A) A = tmp_A
    
  end subroutine qr_decomposition_with_pivot

end program test_qr
