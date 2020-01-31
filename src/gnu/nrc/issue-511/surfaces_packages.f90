module package_interface
  implicit none

  type flux_planes
    integer, allocatable :: values(:,:)
  end type

  type package
    integer id
    type(flux_planes), allocatable  :: fluxes(:)
    integer, allocatable :: positions(:,:)
  end type

end module package_interface

module surfaces_interface
  use package_interface, only : package
  implicit none

  type surfaces
    type(package), allocatable :: halo_outbox(:,:)
  contains
    procedure, nopass :: set_halo_outbox
    procedure, nopass :: get_surface_normal_spacing
  end type

  interface
    module subroutine set_halo_outbox(my_halo_outbox)
      implicit none
      type(package), intent(in) :: my_halo_outbox(:,:)
    end subroutine

    module subroutine get_surface_normal_spacing(image, id, face)
      implicit none
      integer, intent(in) :: image, id, face
    end subroutine
  end interface

end module

submodule(surfaces_interface) surfaces_implementation
  implicit none
  type(surfaces), save :: singleton[*]
contains

  module procedure get_surface_normal_spacing
    ! correct value:
    print *, "on image", this_image(), " allocated: ", allocated(singleton[image]%halo_outbox(id,face)%positions)
    ! incorrect value on image 1, seg fault on image 2:
    print *, "on image", this_image(), " values (should be 6): ", singleton[image]%halo_outbox(id,face)%fluxes(1)%values(1,1)
    ! seg fault on image 2:
    print *, "on image", this_image(), " shape (should be 2 2): ", shape(singleton[image]%halo_outbox(id,face)%positions)
    ! seg faults even with one image:
    print *, "on image", this_image(), " positions (should be 5): ", singleton[image]%halo_outbox(id,face)%positions(1,1)
  end procedure

  module procedure set_halo_outbox
    singleton%halo_outbox = ( my_halo_outbox ) !! parentheses prevent GCC 8.3 ICE when any(lbound(my_halo)/=1)
    sync all
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
  use problem_discretization_interface, only : problem_discretization
  use surfaces_interface, only : surfaces
  use package_interface, only : package
  implicit none
  type(problem_discretization) global_grid
  type(package), allocatable :: bare(:,:)
  integer i, j, k

  allocate( bare(this_image()+1, 1) )
  do i=1, size(bare,1)
    do j=1, size(bare,2)
      bare(i,j)%id = i
      bare(i,j)%positions = reshape( [5,4,3,2], [2,2] )
      allocate(bare(i,j)%fluxes(1))
      do k=1, size(bare(i,j)%fluxes)
        bare(i,j)%fluxes(k)%values =  reshape( [6], [1,1] )
      end do
    end do
  end do

  call global_grid%block_surfaces%set_halo_outbox(bare)
  call global_grid%block_surfaces%get_surface_normal_spacing(image=1, id=1, face=1)
end program main
