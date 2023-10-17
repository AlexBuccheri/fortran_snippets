module types
  use iso_fortran_env
  
  ! Proper enums would be nice from the standard
  type data_type
     integer :: type_real32 = 1
     integer :: type_real64 = 2
  end type data_type

  ! Easier than making private attributes and adding getters
  type(data_type), protected :: data_types =  data_type()

contains
 
  function determine_type(input_value) result(enum_integer)
    class(*), intent(in) :: input_value(:)
    integer :: enum_integer

    select type(input_value)
    type is ( real(kind=REAL32) )
       enum_integer = data_types%type_real32
       return
       
    type is ( real(kind=REAL64) )
       enum_integer = data_types%type_real64
       return

    ! Extend as appropriate   
        
    end select
   
  end function determine_type

  
end module types


program check_datatype
  use iso_fortran_env
  use types
  real(real64) :: a(3)
  complex(real64) :: b(3)

  write(*, *) determine_type(a) == data_types%type_real32
  write(*, *) determine_type(a) == data_types%type_real64
  write(*, *) determine_type(b) == data_types%type_real64

end program check_datatype
