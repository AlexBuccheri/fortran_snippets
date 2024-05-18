! ------------------------------------
! Perform pseudo inversion using SVD
! ------------------------------------
!
! For the example A, expect A_inv =
!  [[-0.2    0.175 -0.025]
!   [-0.1    0.025  0.425]
!   [ 0.3   -0.075 -0.275]]
!
! Compilation on Mac: gfortran -framework Accelerate inversion.f90 -o invert
!
! Reproducing the same operations with python
!
!```python
!  import numpy as np
!
!  a = np.array([[2., 4., 6.],
!                [8., 5., 7.],
!                [0., 3., 1.]])
!
!  # 1. Implement with SVD
!  U, S, Vh = np.linalg.svd(a, full_matrices=True)
!
!  S_inv = np.zeros(shape=(3,3))
!  np.fill_diagonal(S_inv, 1. / S)
!
!  # A^+ = V S^+ UT
!  VSi = (Vh.T @ S_inv)
!  print(VSi)
!  a_inv = VSi @ U.T
!  print(a_inv)
!
!  # 2. Verify against np
!  B = np.linalg.pinv(a)
!  print(B)
!```
program inversion
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none

  integer, parameter :: m = 3
  integer, parameter :: n = 3
  real(dp) :: A(m, n),  A_inv(n, m)
  integer :: i
  
  A = transpose(reshape(&
       [2., 4., 6.,     &
        8., 5., 7.,     &
        0., 3., 1.], [m,n]))

  call pseudo_inv(A, A_inv)

  do i = 1, n
     write(*, *) A_inv(i, :)
  enddo
     

contains

  !> @brief Compute pseudo-inv with SVD
  subroutine pseudo_inv(A, A_inv)
    real(dp),              intent(in)   :: A(:, :)
    real(dp),              intent(out)  :: A_inv(:, :)

    real(dp), allocatable :: u(:, :), S(:), vt(:, :) !< Result of SVD on A
    real(dp), allocatable :: s_inv(:, :)             !< inverse of the diagonals of S
    real(dp), allocatable :: VSi(:, :)               !< Contraction of V and inverse(S)
    integer :: i
    
    call svd(A, U, S, VT)

    ! Compute inverse of A: A^+ = V S^+ U^T

    ! Invert the diagonals of S, to give S^+
    ! TODO. Look into refactoring to see if I can use a vector instead of a diagonal matrix
    allocate(s_inv(m, n), source=0._dp)
    do i = 1, size(s, 1)
       s_inv(i, i) = 1._dp / s(i)
    enddo
    deallocate(S)

    ! NOTE, I "think" I have the leading dimensions correct when requesting the OP on a transpose
    ! As the test case is square, it may be masking an error
    ! Contract (V S^+) = VS
    allocate(VSi(n, n))
    call dgemm('T', 'N', size(VT, 1), size(s_inv, 2), size(VT, 2), 1._dp, VT, size(VT, 2), s_inv, size(s_inv, 1), 0._dp, VSi, n)
    deallocate(VT)

    ! Contract A_inv = (V S^+) U^T
    call dgemm('N', 'T', size(VSi, 1), size(U, 2), size(VSi, 2), 1._dp, VSi, size(VSi, 1), U, size(U, 2), 0._dp, A_inv, m)

    deallocate(VSi)
    deallocate(U)
        
  end subroutine pseudo_inv


  !> @brief SVD wrapper
  !!
  !! Workspace query size taken from:
  !! https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/examples/source/dgesvd_example.f90
  subroutine svd(A, U, S, VT)
    real(dp),              intent(in)  :: A(:, :)
    real(dp), allocatable, intent(out) :: u(:, :)  !< U matrix
    real(dp), allocatable, intent(out) :: S(:)     !<  min(m,n) singular values of A
    real(dp), allocatable, intent(out) :: vt(:, :) !< V^T

    real(dp), allocatable :: work(:)
    real(dp) :: dummy(1, 1)
    integer, parameter :: nb = 64
    integer :: lda, ldu, ldvt, info, lwork

    !                     A    =     U       S       VT
    ! with shape:       (m,n)  =  (m, m)  (m, n)   (n, n) 
    ! which reduces to: (m,n)  =  (m, m) min(m, n) (n, n)
    ! if S is represented as a vector.
    lda = size(A, 1)
    ldu = size(A, 1)
    ldvt = size(A, 2)
    
    allocate(s(min(m, n)))
    allocate(u(ldu, m))
    allocate(vt(ldvt, n))

    ! Query optimal work space
    lwork = -1
    call dgesvd('A', 'S', m, n, A, lda, s, u, ldu, vt, ldvt, dummy, lwork, info)

    ! Compute the singular values and left and right singular vectors of A.
    lwork = max(m+4*n+nb*(m+n), nint(dummy(1,1)))
    allocate(work(lwork))
    call dgesvd('A', 'S', m, n, A, lda, s, u, ldu, vt, ldvt, work, lwork, info)
    
    if (info/=0) then
       write (*, *) 'Failure in DGESVD. INFO =', info
    endif

    deallocate(work)

  end subroutine svd

end program inversion
