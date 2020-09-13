module results_interface
  use oracle_interface, only : oracle
  implicit none

  private

  type, public, extends(oracle) :: results_t
    private
    character(len=:), allocatable :: header(:)
    real, allocatable :: body(:,:)
  contains
    procedure :: write_formatted
    procedure :: subtract
  end type

  interface

    module subroutine write_formatted(this, unit, iotype, vlist, iostat, iomsg)
      implicit none
      class(results_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine

    module function subtract(this, rhs) result(difference)
      !! result has components corresponding to subtracting rhs's components fron this object's components
      implicit none
      class(results_t), intent(in) :: this
      class(oracle), intent(in) :: rhs
      class(oracle), allocatable :: difference
    end function

  end interface

end module results_interface
