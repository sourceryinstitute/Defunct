module co_object_interface
  implicit none

  private
  public :: co_object

  type, abstract :: co_object
    private
    logical, allocatable :: facilitate_type_extension[:]
  end type

end module
module grid_interface
  use co_object_interface, only : co_object
  implicit none

  private
  public :: grid

  type, extends(co_object) :: grid
  end type

end module
module structured_grid_interface
  use grid_interface, only : grid
  implicit none

  private
  public :: structured_grid

  type, extends(grid) :: structured_grid
     private
     real, allocatable :: nodal_values(:,:,:,:,:,:)[:,:,:]
  end type

  interface
    module subroutine create_from_file(this)
      implicit none
      class(structured_grid), intent(out) :: this
    end subroutine
  end interface

end module
