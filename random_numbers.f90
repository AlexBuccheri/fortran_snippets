! Basic random number generation
! Not thread or MPI-safe
! gfortran seeding.f90 -o seed  && ./seed
program random_numbers
    use, intrinsic :: iso_fortran_env
    implicit none
  
    integer, allocatable :: random_integers(:)
    integer, allocatable :: seed(:)
    integer :: i
    integer :: min_seed_size, n, max_integer

    ! Number of random numbers to return
    n = 10

    ! Max integer to return, [1:max_integer]
    max_integer = 20

    ! Query the seed size. This seems to be implementation-specific
    call random_seed(size=min_seed_size) 
    ! write(*, *) "Required minimum seed size:", min_seed_size

    ! Fix seed to fix random number generation
    allocate(seed(min_seed_size), source=[(i, i=1, min_seed_size)])
    allocate(random_integers(n))
    
    write(*, *) 'Fixed set of n random integers [1:max_integer]'
    call generate_random_integers(max_integer, random_integers, seed=seed)
    write(*, *) random_integers

    write(*, *) 'Varying set of n random integers [1:max_integer]'
    call generate_random_integers(max_integer, random_integers)
    write(*, *) random_integers


contains 

    !> @brief Generate an array of random integers in the range [1:max_integer]
    subroutine generate_random_integers(max_integer, random_integers, seed)
        integer, intent(in)           :: max_integer
        integer, intent(out)          :: random_integers(:)
        integer, intent(in), optional :: seed(:)

        integer :: i
        real(real64) :: random_num 

        if (present(seed)) then
            call random_seed(put=seed)  
        else
            call random_seed()
        endif  

        do i = 1, size(random_integers)
            ! Generate random number between 0 and 1
            call random_number(random_num) 
            random_integers(i) = int(random_num * max_integer + 1, kind=int32)
        end do

    end subroutine generate_random_integers

end program random_numbers
