  logical function is_symmetric(a, tol)
    real(real64),           intent(in)  :: a(:, :)
    real(real64), optional, intent(in)  :: tol

    real(real64), allocatable :: diff(:, :)
    integer                   :: n, i, j, j_block, i_block, j_end, i_end, block_size
    real(real64)              :: tolerance

    PUSH_SUB(is_symmetric)

    tolerance = optional_default(tol, 1.0e-8_real64)
    n = size(a, 1)
    ! a must be square
    ASSERT(size(a, 2) == n)

    SAFE_ALLOCATE(diff(1:n, 1:n))

    ! Blocked-up implementation such that block_size**2 elements fit into cache
    block_size = floor( sqrt(real(cpu_hardware%l1%size /sizeof_real64, real64)) )

    ! Criterion established empirically. For n/block_size > 30, cache-friendly looping
    ! is ~ 2x faster. Tiling tends to be faster even if the l1 cache size used is
    ! 50% (100%) smaller (larger) than actually available
    if (n > 30 * block_size) then
      do j_block = 1, n, block_size
        j_end = min(j_block + block_size - 1, n)
        do i_block = 1, n, block_size
          i_end = min(i_block + block_size - 1, n)
          do j = j_block, j_end
            do i = i_block, i_end
              diff(i, j) = abs(a(i, j) - a(j, i))
            end do
          end do
        end do
      end do
    else
      do j = 1, n
        do i = 1, n
          diff(i, j) = abs(a(i, j) - a(j, i))
        end do
      end do
    endif

    is_symmetric = all(diff < tolerance)
    SAFE_DEALLOCATE_A(diff)

    POP_SUB(is_symmetric)

  end function is_symmetric
