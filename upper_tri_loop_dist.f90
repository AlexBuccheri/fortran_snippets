! Some notes on distributing double-loop over up triangle
! gfortran -fopenmp -I/opt/local/lib/gcc12/gcc/arm64-apple-darwin21/12.3.0/finclude/ loop_dist.f90 -o lo   

program loop_dist
  use omp_lib
  implicit none
  integer, parameter :: natoms = 32
  real(8) :: positions(3, natoms) = reshape([ &
       0.000000, 0.000000, 0.000000, &
       0.000000, 0.000000, 0.500000, &
       0.000000, 0.500000, 0.000000, &
       0.000000, 0.500000, 0.500000, &
       0.500000, 0.000000, 0.000000, &
       0.500000, 0.000000, 0.500000, &
       0.500000, 0.500000, 0.000000, &
       0.500000, 0.500000, 0.500000, &
       0.250000, 0.250000, 0.250000, &
       0.250000, 0.250000, 0.750000, &
       0.250000, 0.750000, 0.250000, &
       0.250000, 0.750000, 0.750000, &
       0.750000, 0.250000, 0.250000, &
       0.750000, 0.250000, 0.750000, &
       0.750000, 0.750000, 0.250000, &
       0.750000, 0.750000, 0.750000, &
       0.125000, 0.125000, 0.125000, &
       0.125000, 0.125000, 0.625000, &
       0.125000, 0.625000, 0.125000, &
       0.125000, 0.625000, 0.625000, &
       0.625000, 0.125000, 0.125000, &
       0.625000, 0.125000, 0.625000, &
       0.625000, 0.625000, 0.125000, &
       0.625000, 0.625000, 0.625000, &
       0.375000, 0.375000, 0.375000, &
       0.375000, 0.375000, 0.875000, &
       0.375000, 0.875000, 0.375000, &
       0.375000, 0.875000, 0.875000, &
       0.875000, 0.375000, 0.375000, &
       0.875000, 0.375000, 0.875000, &
       0.875000, 0.875000, 0.375000, &
       0.875000, 0.875000, 0.875000], [3, natoms])
  integer :: ia, ja, i,j,k, N
  real(8) :: energy
  
  ! Serial loop
  energy = 0.d0
  do ia = 1, natoms
     do ja = ia +1, natoms
        energy = energy + (1. / norm2(positions(:, ia) - positions(:, ja)) )
     enddo
  enddo
  write(*, *) 'energy', energy
  
  ! Loop distribution with OMP
  ! Works from outside in
  ! Unfortunately the loops must form a rectangular iteration space, which
  !  do ia = 1, natoms
  !   do ja = ia +1, natoms
  ! does not.

  ! Unrolling this loop is extremely non-trivial
  !k = 0
  !do i = 1, N
  !   do j = i, N
  !      k = k + 1
  !      write(*, *) 

  ! Can only distribute the outer loop, which leads to the same terrible load-balancing
  ! that MPI provides
  ! So should probably try the block-cyclic distribution
  ! Only although alternative is to generate all permutations for the upper triange i.e
  ! 1,2 1,3 1,4 ...2,3 2,4 ... which requires 0.5N(N+1) operations first
  energy = 0.d0
  !$omp parallel do private(ia, ja) reduction(+:energy)
  do ia = 1, natoms
     do ja = ia +1, natoms
        energy = energy + (1. / norm2(positions(:, ia) - positions(:, ja)) )
     enddo
  enddo
  write(*, *) 'energy', energy
  
  
end program loop_dist
