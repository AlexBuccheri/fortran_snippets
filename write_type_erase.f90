!! Copyright (C) 2026. Alexander Buccheri
!! alexanderbuccheri@googlemail.com
!!
!! This program is free software; you can redistribute it and/or modify
!! it under the terms of the GNU General Public License as published by
!! the Free Software Foundation; either version 2, or (at your option)
!! any later version.
!!
!! This program is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU General Public License for more details.
!!
!! You should have received a copy of the GNU General Public License
!! along with this program; if not, write to the Free Software
!! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
!! 02110-1301, USA.
!!

! Compile: gfortran write_type_erase.f90 -o wrt
! Run: ./wrt

module observation_m
  implicit none
  public

  ! Interfaces of methods that an object must have to be registered with observation_slot_t instance
  abstract interface
    logical function obs_write_status(obj)
      class(*), intent(in) :: obj
    end function obs_write_status

    subroutine obs_write(obj)
      class(*), intent(in) :: obj
    end subroutine obs_write
  end interface

  ! Type-erased registry slot: object pointer + dispatcher callbacks,
  ! allowing heterogeneous objects with specific methods (in this case write)
  ! to be stored in one array.
  type observation_slot_t
    character(len=:),        allocatable :: label
    class(*),                    pointer :: obj => null()
    procedure(obs_write_status), pointer, nopass :: obj_write_status => null()
    procedure(obs_write),        pointer, nopass :: obj_write => null()
  contains
    final :: observation_slot_finalize
    procedure :: write_status 
    procedure :: write
  end type observation_slot_t

contains

  logical function write_status(this)
    class(observation_slot_t), intent(in) :: this
    ! Should assert associated(this%obj)
     write_status = this%obj_write_status(this%obj)
  end function write_status

  subroutine write(this)
    class(observation_slot_t), intent(in) :: this
    call this%obj_write(this%obj)
  end subroutine write

  subroutine observation_slot_finalize(this)
    type(observation_slot_t), intent(inout) :: this

    if (allocated(this%label)) deallocate(this%label)
    nullify(this%obj)
    nullify(this%obj_write_status)
    nullify(this%obj_write)
  end subroutine observation_slot_finalize

end module observation_m


module observations_m
  use observation_m, only: observation_slot_t, obs_write_status, obs_write
  implicit none
  public :: observations_t

  ! Registry class to store several observation_slot_t instances
  type observations_t
    type(observation_slot_t), allocatable :: slots(:)
  contains
    final :: observations_finalize
    procedure :: init => observations_init
    procedure :: set => observations_set
  end type observations_t

contains

  subroutine observations_init(this, n_slots)
    class(observations_t), intent(inout) :: this
    integer,               intent(in)    :: n_slots
    ! Should assert n_slots > 0
    allocate(this%slots(n_slots))
  end subroutine observations_init

  ! Wrapper to the initialiser/constructor of the `islot` observation_slot_t instance
  subroutine observations_set(this, islot, label, obj, obj_write_status, obj_write)
    class(observations_t), intent(inout) :: this
    integer,               intent(in)    :: islot
    character(len=*),      intent(in)    :: label
    class(*), target,      intent(in)    :: obj
    procedure(obs_write_status)          :: obj_write_status !< write_status method of obj
    procedure(obs_write)                 :: obj_write        !< write method of obj

    if (.not. allocated(this%slots)) then
      error stop "observations_set: observations_init not called"
    endif

    ! This can be handled dynamically, but leave as a fixed initialisation
    ! value for now
    if (islot > size(this%slots)) then
      error stop "observations_set: slot index exceeds n_slots used to initialisation"
    endif

    ! Use the default constructor
    this%slots(islot) = observation_slot_t(label, obj, obj_write_status, obj_write)

  end subroutine observations_set

  subroutine observations_finalize(this)
    type(observations_t), intent(inout) :: this
    if (allocated(this%slots)) deallocate(this%slots)
  end subroutine observations_finalize

end module observations_m


! A simple class with the correct method signatures
module class_m
  implicit none

  public :: my_class_t, my_class_write_status_dispatcher, my_class_write_dispatcher

  type my_class_t
     integer :: itr
     character(len=250) :: message
   contains
     procedure :: write_status
     procedure :: write
  end type my_class_t

contains

  subroutine write(this)
    class(my_class_t), intent(in) :: this
    write(*, *) trim(this%message), this%itr
  end subroutine write

  ! Only write for even integer iterations
  logical function write_status(this)
    class(my_class_t), intent(in) :: this
    write_status = mod(this%itr, 2) == 0
  end function write_status

  ! Extra boiler plate required to dispatch the generic object methods
  ! Better here (i.e. extra boilerplate next to each class), than
  ! written once and constantly extended in the set method of `observations_t`
  logical function my_class_write_status_dispatcher(obj)
    class(*), intent(in) :: obj

    select type (obj)
    type is (my_class_t)
      my_class_write_status_dispatcher = obj%write_status()
    class default
      error stop "my_class_write_status_dispatcher: unexpected type"
    end select

  end function my_class_write_status_dispatcher

  subroutine my_class_write_dispatcher(obj)
    class(*), intent(in) :: obj

    select type (obj)
    type is (my_class_t)
      call obj%write()
    class default
      error stop "my_class_write_dispatcher: unexpected type"
    end select

  end subroutine my_class_write_dispatcher

end module class_m


! Test program
program write_abstract
  use observations_m
  use class_m
  implicit none

  type(my_class_t), target :: obj
  type(observations_t) :: registry
  integer :: i, iwrite, n_objs
  integer, parameter :: n_steps = 10

  obj = my_class_t(0, "Only write on even itr")

  ! Directly call the obj methods
  write(*, *) 'Directly call the obj methods for writing:'
  do i = 1, n_steps
     obj%itr = i
     if(obj%write_status()) call obj%write()
  enddo
  write(*, *)

  ! Call via a dispatcher/registry
  n_objs = 1
  call registry%init(n_objs)
  call registry%set(1, "my_class_instance", obj, my_class_write_status_dispatcher, my_class_write_dispatcher)

  write(*, *) 'Call via the registry:'
  do i = 1, n_steps
     obj%itr = i
    do iwrite = 1, size(registry%slots)
      if (registry%slots(iwrite)%write_status()) then
        call registry%slots(iwrite)%write()
      endif
    end do
  enddo

end program write_abstract
