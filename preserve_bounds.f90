! For an array declared:
! ```
! a(start:end)  ! where start > 1
! ```
! one has to declare the dummy argument allocatable or
! ```
! real(real64), intent(in) :: a(start:)
! ```
! to ensure the lower bound is preserved
!
! mpif90 -g -fcheck=all preserve_bounds.f90  -o preserve_bounds
! mpirun -np 2 preserve_bounds

module routines
  use mpi
  implicit none

  ! Same on every process
  integer :: nk_local = 4

contains

  subroutine set_p(p)
    integer, allocatable, intent(out) :: p(:)

    integer :: kstart, kend
    integer :: rank, ierr

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    kstart = 1 + rank * nk_local
    kend   = nk_local + rank * nk_local

    allocate(p(kstart:kend), source=rank)
    write(*, *) "Set Bounds", lbound(p), ubound(p)

  end subroutine set_p


  subroutine run_over_p(p)
    ! Must be allocatable to preserve bounds
    integer, allocatable, intent(in) :: p(:)

    integer :: kstart, kend, ik, iprocess, nprocesses
    integer :: rank, ierr

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, nprocesses, ierr)

    kstart = 1 + rank * nk_local
    kend   = nk_local + rank * nk_local

    do iprocess = 0, nprocesses - 1
       if (iprocess == rank) then
          write(*, *) "local to rank ", rank
          write(*, *) "Read Bounds", lbound(p), ubound(p)

          do ik = kstart, kend
             write(*, *) ik, p(ik)
          end do
       endif

       call MPI_Barrier(MPI_COMM_WORLD, ierr)
    enddo

  end subroutine run_over_p

end module routines



program test_routines
  use mpi
  use routines
  implicit none

  integer :: ierr
  integer, allocatable :: p(:)

  call MPI_Init(ierr)

  call set_p(p)
  call run_over_p(p)

  call MPI_Finalize(ierr)

end program test_routines
