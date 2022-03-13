!> Fill an array with an implied do loop, which includes a function call.
program implieddop
  implicit none
  integer, dimension(5) :: imap = [5, 4, 3, 2, 1]
  real(8) :: array(5)
  integer :: i

  array = [(modify(imap(i)), i = 1, size(imap))]
  write(*,*) array

contains

  !> Some arbitrary function that returns input * 2.
  function modify(a) result(product)
    integer, intent(in) ::a
    real(8) :: product
    product = a * 2.d0
  end function modify

end program implieddop
