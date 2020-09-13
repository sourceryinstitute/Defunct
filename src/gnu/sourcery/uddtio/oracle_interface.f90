module oracle_interface
  use object_interface, only : object
  use kind_parameters, only : rkind
  implicit none

  private
  public :: oracle

  type, abstract, extends(object) :: oracle
  contains
    procedure(subtract_interface), deferred :: subtract
    generic :: operator(-) => subtract
  end type

  abstract interface
    function subtract_interface(this, rhs) result(difference)
      import oracle
      implicit none
      class(oracle), intent(in) :: this, rhs
      class(oracle), allocatable :: difference
    end function
  end interface

end module
