! Two approaches to wrapping a type and its methods in my own derived type
! Useful for when one may have a choice of libraries that do the same thing
! or a library changes the naming or API for methods of an object.. 
module wrapper
  implicit none
  private

  ! Some base type with methods, that one would typically load from a library
  type, private :: base_type
     logical :: base_condition = .false.
   contains
     private
     procedure :: base_assert
  end type base_type

  
  ! Wrapper class
  ! Also don't want this to inheret methods from the base class but not sure I can prevent that
  ! as I want to use a binding name for the method that differs from the base class.
  !
  ! "An extended type may override methods from the base type. The method is overridden when the
  ! binding name of a procedure in the extensible class is the same as the binding name in the parent class."
  type, public, extends(base_type) :: my_type
     logical :: my_condition = .false.
   contains
     ! Binding name and procedure name 
     procedure, public :: assert => my_assert
  end type my_type 

  
  ! Alternatively instantiate a private instance of the base class
  ! and use this in the module, accessing its methods via my public API
  ! for my_second_type
  type(base_type), private :: base_instance 
  type, public :: my_second_type
     logical :: my_condition = .false.
   contains 
     procedure, public :: assert => my_other_assert
  end type my_second_type
  
  
  
contains

  ! Method of the base_type (would be hidden in external lib)
  subroutine base_assert(this, condition) 
    class(base_type), intent(inout) :: this
    logical, intent(in) :: condition
    this%base_condition = condition
  end subroutine base_assert

  ! My procedure is just a wrapper for the method of the base class
  ! In this case, it sets a variable in the base class, then uses that
  ! variable to set the equivalent in my class
  subroutine my_assert(this, condition) 
    class(my_type), intent(inout) :: this
    logical, intent(in) :: condition
    call this%base_type%base_assert(condition)
    this%my_condition = this%base_type%base_condition
  end subroutine my_assert

  ! Rather than extend the base type, instantiate an instance of it
  ! in the module and restrict it to this namespace
  ! Only access its data/methods via my public API in my_second_type
  subroutine my_other_assert(this, condition)
    class(my_second_type), intent(inout) :: this
    logical, intent(in) :: condition
    call base_instance%base_assert(condition)
    this%my_condition = base_instance%base_condition 
  end subroutine my_other_assert
  
end module wrapper


program wrapper_test
  use wrapper, only: my_type, my_second_type
  implicit none
  type(my_type) :: test
  type(my_second_type) :: test2

  write(*,*) test%my_condition 
  call test%assert(.true.)
  write(*,*) test%my_condition, test%base_condition 

  ! This approach is preferred as I don't have access to public
  ! data in the base class directly from test2 i.e. smaller scope
  write(*,*) test2%my_condition
  call test2%assert(.true.)
  write(*,*) test2%my_condition, test%base_condition 
  
end program wrapper_test
