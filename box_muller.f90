 !The Box–Muller transform (or “Box–Muller algorithm”) is a method for
 !turning pairs of independent uniform random numbers into pairs of independent
 !standard normally distributed (Gaussian) random numbers.
program box_muller
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none

  real(real64), allocatable :: G(:, :)
  real(real64) :: g_mean, g_second_m, g_skew
  
  allocate(G(2000, 2000))
  write(*, *) 'even row, odd column'
  call gaussian_sampling_bm(G)
  g_mean = mean(G)
  g_second_m = variance(G, g_mean)
  g_skew = skew(G, g_mean, g_second_m)
  write(*, *) 'Expected Mean, variance    0.0       1.0'
  write(*, *) 'Mean, variance', g_mean, g_second_m, g_skew

  call visualise_distribution(G)
  deallocate(G)

  !allocate(G(3, 2))
  !write(*, *) 'odd row, even column'
  !call gaussian_sampling_bm(G)
  !deallocate(G)

  
  !allocate(G(3, 3))
  !write(*, *) 'odd row, odd column'
  !call gaussian_sampling_bm(G)
  !deallocate(G)

  
  !allocate(G(2, 2))
  !write(*, *) 'even row, even column'
  !call gaussian_sampling_bm(G)
  !deallocate(G)
  
  
contains

  subroutine gaussian_sampling_bm(G)
    real(real64), intent(out) :: G(:,:)

    real(real64), parameter :: pi = 3.141592653589793238462643_real64
    integer      :: i, j, nrow, ncol
    real(real64) :: u1, u2, coeff, theta
    
    nrow = size(G, 1)
    ncol = size(G, 2)
    
    do j = 1, ncol
       i = 1
       do while (i + 1 <= nrow)
          ! Generate two random numbers, and transform
          call random_number(u1)
          call random_number(u2)
          ! Use max to avoid log(0)
          coeff = sqrt(-2.0_real64 * log(max(u1, 1.e-15)))
          theta = 2.0_real64 * pi * u2
          G(i, j)   = coeff * cos(theta)
          G(i+1, j) = coeff * sin(theta)
          !write(*, *) i, j
          !write(*, *) i+1, j
          i = i + 2
       enddo
       ! If there's one row left (nrow odd)
       if (i <= nrow) then
          call random_number(u1)
          call random_number(u2)
          coeff = sqrt(-2.0_real64 * log(max(u1, 1.e-15)))
          theta = 2.0_real64 * pi * u2
          G(nrow, j) = coeff * cos(theta)
          !write(*, *) i, j
       endif
    enddo
    
  end subroutine gaussian_sampling_bm


  real(real64) function mean(x)
    real(real64), intent(in) :: x(:, :)

    integer :: i,j
    real(real64) :: acc

    acc = 0._real64
    do j = 1, size(x, 2)
       do i = 1, size(x, 1)
          acc = acc + x(i, j)
       enddo
    enddo

    mean = acc / real(size(x), real64)
    
  end function mean

  
  real(real64) function variance(x, mean)
    real(real64), intent(in) :: x(:, :)
    real(real64), intent(in) :: mean

    integer :: i,j
    real(real64) :: acc

    acc = 0._real64
    do j = 1, size(x, 2)
       do i = 1, size(x, 1)
          acc = acc + (x(i, j) - mean)**2
       enddo
    enddo

    variance = acc / real(size(x), real64)
    
  end function variance

  
  real(real64) function skew(x, mean, var)
    real(real64), intent(in) :: x(:, :)
    real(real64), intent(in) :: mean
    real(real64), intent(in) :: var

    integer :: i,j
    real(real64) :: acc, sd, third_moment

    acc = 0._real64
    do j = 1, size(x, 2)
       do i = 1, size(x, 1)
          acc = acc + (x(i, j) - mean)**3
       enddo
    enddo

    sd = sqrt(var)
    third_moment = acc / real(size(x), real64)
    skew = third_moment / sd**3
    
  end function skew

  
  subroutine visualise_distribution(X)
    real(real64), intent(in) :: x(:, :)

    integer,      parameter :: nbins=100
    real(real64), parameter :: xmin=-4.0_real64, xmax=4.0_real64
    integer,    allocatable :: hist(:)
    real(real64)            :: xi, dx
    integer                 :: ib, i, j

    allocate(hist(nbins), source=0)
    dx = (xmax - xmin) / nbins
    
    do j = 1, size(X,2)
       do i = 1, size(X,1)
          xi = X(i, j)
          if (xi >= xmin .and. xi < xmax) then
             ! Maps x value to corresponding index
             ib = int( (xi - xmin) / dx ) + 1
             hist(ib) = hist(ib) + 1
          end if
       end do
    enddo
    
    ! Turn count into a probability density
    open(unit=100, file='hist.dat', status='replace')

    do ib = 1, nbins
       xi = xmin + (ib-1)*dx
       write(100, *) xi, hist(ib)
    enddo
    
    close(100)
    
  end subroutine visualise_distribution
  
  
end program box_muller
