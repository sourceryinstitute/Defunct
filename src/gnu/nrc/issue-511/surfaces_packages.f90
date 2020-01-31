module package_interface
  implicit none

  type flux_planes
    real, allocatable, dimension(:,:) :: fluxes
  end type

  type package
    integer :: neighbor_block_id
    integer :: step
    type(flux_planes), allocatable, dimension(:) :: surface_normal_fluxes
    real, allocatable, dimension(:,:,:,:) :: positions
  end type

end module package_interface

module surfaces_interface
  use package_interface, only : package
  implicit none

  integer, parameter :: forward=2, x_dir=1

  type surfaces
    type(package), allocatable, dimension(:,:,:) :: halo_outbox
  contains
    procedure, nopass :: set_halo_outbox
    procedure, nopass :: get_surface_normal_spacing
  end type

  interface

    module subroutine set_halo_outbox(my_halo_outbox, block_partitions)
      !! define halo_outbox component array
      implicit none
      type(package), intent(in), dimension(:,:,:) :: my_halo_outbox
      integer, intent(in), dimension(:) :: block_partitions
    end subroutine

    !pure
    module function get_surface_normal_spacing(neighbor_image, local_neighbor_id, coordinate_direction, neighbor_face) &
      result(dx_normal)
      !! result is the distance to the nearest plane inside the specified block at the specified boundary
      implicit none
      integer, intent(in) :: neighbor_image, local_neighbor_id, coordinate_direction
      integer, intent(in) :: neighbor_face
      real dx_normal
    end function

  end interface

end module

submodule(surfaces_interface) surfaces_implementation
  implicit none

  type(surfaces), save :: singleton[*]

contains

  module procedure get_surface_normal_spacing
    print *, "element: ",singleton[neighbor_image]%halo_outbox(local_neighbor_id,forward,x_dir)%positions(1,1,1,1)
  end procedure

  module procedure set_halo_outbox
    integer b,dir,face
    singleton%halo_outbox = ( my_halo_outbox ) !! parentheses prevent GCC 8.3 internal compiler error when any(lbound(my_halo...)/=1)
    sync all !! ensure all images have defined halo_outboxes (however, some components
  end procedure

end submodule

module problem_discretization_interface
  use surfaces_interface, only : surfaces
  implicit none
  type problem_discretization
    type(surfaces) block_surfaces
  end type
end module

program main
  implicit none
end program main
