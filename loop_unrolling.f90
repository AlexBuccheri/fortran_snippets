!> Program demonstrating arbitrary loop unrolling given a composite index,
!> assumed loop nesting order and the limits of those loops.
!>
!> Explicit expressions are given for the common cases of unrolling (i, j) and (i, j, k). 
!>
!> In all tests we expect zeros, implying the computed limits match the actual limits.
program loop_unrolling
  implicit none

  !--------------------
  ! Declarations
  !--------------------
  integer :: i, j, n, m, cnt, i_unroll, j_unroll, k, o, n_max, m_max, l_max, k_max, j_max, i_max, l, &
       inner, middle, outer
  integer, allocatable :: indices(:), limits(:)

  !--------------------
  ! Tests
  !--------------------
  
  write(*, *) 'Single nested example with the explicit expression:'
  n = 2
  m = 3
  
  do i = 1, n
     do j = 1, m
        cnt = j + (i-1) * m
        call unroll_single_nested_loop(cnt, m, inner, outer)
        write(*,*)  [i, j] - [outer, inner]
     enddo
  enddo
  

  write(*, *) 'Single nested example with the generic subroutine:'
  allocate(indices(2), limits(2))
  limits = [m, n]
  
  do i = 1, n
     do j = 1, m
        cnt = j + (i-1) * m
        call unroll(cnt, limits, indices)
        write(*,*)  [j, i] - indices
     enddo
  enddo
  
  deallocate(indices, limits)


  write(*,*) 'Triple nested example with the explicit expression:'
  n = 2
  m = 3
  o = 3  
  allocate(limits(3), source=[o, m, n])

  cnt = 0
  do i = 1, n
     do j = 1, m
        do k = 1, o
           cnt = cnt + 1
           call unroll_composite_to_ijk(cnt, limits, inner, middle, outer)
           write(*,*)  [k, j, i] - [inner, middle, outer]
        enddo
     enddo
  enddo


  write(*,*) 'Triple nested example with the generic subroutine:'
  allocate(indices(3))
  cnt = 0
  do i = 1, n
     do j = 1, m
        do k = 1, o
           cnt = cnt + 1
           call unroll(cnt, limits, indices)
           write(*,*)  [k, j, i] - indices
        enddo
     enddo
  enddo
  deallocate(indices, limits)
  

  write(*,*) '6 loops from inner to outer: (n, m, l, k, j, i)'
  allocate(limits(6), indices(6))
  n_max = 2
  m_max = 1
  l_max = 3
  k_max = 5
  j_max = 2
  i_max = 3
  limits = [n_max, m_max, l_max, k_max, j_max, i_max]
  
  cnt = 0
  do i = 1, i_max
     do j = 1, j_max
        do k = 1, k_max
           do l = 1, l_max
              do m = 1, m_max
                 do n = 1, n_max
                    cnt = cnt + 1
                    call unroll(cnt, limits, indices)
                    write(*,*) [n, m, l, k, j, i] - indices(:)
                 enddo
              enddo
           enddo
        enddo
     enddo
  enddo

  
contains

  !> Given a composite index composed of (i, j, k, l, ...) with limits (n, m, o, p, ...),
  !> return the values of  (i, j, k, l, ...), where the loop ordering is innermost to outermost, going from left to right:
  !>
  !> cnt = 0
  !> do l = 1, p
  !>  do k = 1, o
  !>   do j = 1, m
  !>    do i = 1, n
  !>      cnt = cnt + 1
  !>
  !> For a given cnt, return indices = [i, j, k, l] from unroll.
  !>
  !> The recursive strategy is explained in the documentation to: subroutine unroll_composite_to_ijk
  !
  subroutine unroll(input_cmp, limits, indices)
    !> Composite index
    integer, intent(in) :: input_cmp
    !> Loop limits for each index, ordered from innermost at limit(1) to outermost at limit(n_loops)
    integer, intent(in) :: limits(:)
    !> Indices that compose the composite index cmp, ordered from innermost at limit(1) to outermost at limit(n_loops)
    integer, intent(out) :: indices(:)

    integer :: il, limit, cmp, inner, outer
    
    ! Assert size(limits) == size(indices)
    cmp = input_cmp
    
    do il = 1, size(limits)
       limit = limits(il)
       call unroll_single_nested_loop(cmp, limit, inner, outer)
       indices(il) = inner
       cmp = outer
    enddo
    
  end subroutine unroll

  
  !> Return the inner and outer indices that compose the total index of a nested loop.
  !>
  !> For example:
  !> cmp = 0
  !> do i = 1, n
  !>  do j = 1, limit
  !>    cmp = cmp + 1
  !>
  !> For a value of cmp, return the corresponding (i, j)
  subroutine unroll_single_nested_loop(cmp, inner_limit, inner, outer)
    !> Composite index
    integer, intent(in) :: cmp
    !> Limit of the inner loop
    integer, intent(in) :: inner_limit
    !> Unrolled indices of the inner and outer loops, for a given cnt. 
    integer, intent(out) :: inner, outer
    outer = int((cmp - 1) / inner_limit) + 1
    inner = cmp - (outer-1) * inner_limit
  end subroutine unroll_single_nested_loop


  !> Given a composite index associated with 3 nested loops, return the
  !> individual loop indices (k, j, i)
  !>
  !> The limits are expected from innermost to outermost: [Nk, Nj, Ni]
  !> The indices are returned from innermost to outermost.
  !> 
  !> For example:
  !> cmp = 0
  !> do i = 1, Ni
  !>   do j = 1, Nj
  !>     do k = 1, Nk
  !>       cmp = cmp + 1
  !>
  !> The routine (analytically) works recursively, where the problem is
  !> formulated as a series of single-nested loop unrolls:
  !> 
  !> 1. Treat the inner loop explicitly and combine all other loops into
  !>    a composite loop:
  !>
  !> cmp = 0
  !> do ij = 1, Ni * Nj
  !>   do k = 1, Nk
  !>     cmp = cmp + 1
  !>
  !> Find the composite index ij and the index k associated with the inner loop.
  !> (See the expression in subroutine unroll_single_nested_loop). 
  !> Now one has ij, they can reduce to one pair of loops to solve for the new
  !> outer loop and the remaining inner loop:
  !>
  !> ij = 0
  !> do i = 1, Ni
  !>   do j = 1, Nj
  !>     ij = ij + 1
  !>
  !> returning i, which is no longer a composite index. Finally, i and ij are used
  !> to return the inner loop of this pair/ middle loop of the full loop structure. 
  !> This process is performed recursively for N nested loops by subroutine unroll.
  !
  subroutine unroll_composite_to_ijk(cmp, limits, k, j, i)
    !> Composite loop index
    integer, intent(in) :: cmp
    !> Limits of (k, j, i) = [Nk, Nj, Ni]
    integer, intent(in) :: limits(3)
    !> Loop indices ordered: inner, middle, outer
    integer, intent(out) :: k, j, i

    integer :: ij, nk, nj, ni
    
    nk = limits(1)
    nj = limits(2)
    ni = limits(3)

    ij = int((cmp - 1) / nk) + 1
    k = cmp - (ij - 1) * nk
    
    i = int((ij - 1)/ nj) + 1
    j = ij - (i - 1) * nj
    
  end subroutine unroll_composite_to_ijk

  
end program loop_unrolling
