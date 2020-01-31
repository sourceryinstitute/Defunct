module package_interface
  implicit none


  type package
    integer id
    integer, allocatable :: flux_values(:)
    integer, allocatable :: positions(:)
  end type

end module package_interface

module surfaces_interface
  use package_interface, only : package
  implicit none

  type surfaces
    type(package), allocatable :: halo_outbox(:,:,:)
  contains
    procedure, nopass :: set_halo_outbox
    procedure, nopass :: get_surface_normal_spacing
  end type

  interface
    module subroutine set_halo_outbox(my_halo_outbox)
      implicit none
      type(package), intent(in) :: my_halo_outbox(:,:,:)
    end subroutine

    module subroutine get_surface_normal_spacing
      implicit none
    end subroutine
  end interface

end module

submodule(surfaces_interface) surfaces_implementation
  implicit none
  type(surfaces), save :: singleton[*]
contains

  module procedure get_surface_normal_spacing
    integer b, d, f

    associate( me => this_image() )
      print*, "image", me, " halo_outbox allocated: ", allocated(singleton%halo_outbox)

      do b=1,size(singleton%halo_outbox,1); do d=1,size(singleton%halo_outbox,2); do f=1,size(singleton%halo_outbox,3)
        print *, allocated(singleton%halo_outbox(b,d,f)%flux_values), shape(singleton%halo_outbox(b,d,f)%flux_values)
        print *, allocated(singleton%halo_outbox(b,d,f)%positions), shape(singleton%halo_outbox(b,d,f)%positions)
        print *, singleton%halo_outbox(b,d,f)%positions, singleton%halo_outbox(b,d,f)%flux_values
      end do; end do; end do

      do b=1,size(singleton[1]%halo_outbox,1); do d=1,size(singleton[1]%halo_outbox,2); do f=1,size(singleton[1]%halo_outbox,3)
        print *, allocated(singleton[1]%halo_outbox(b,d,f)%flux_values), shape(singleton[1]%halo_outbox(b,d,f)%flux_values)
        print *, allocated(singleton[1]%halo_outbox(b,d,f)%positions), shape(singleton[1]%halo_outbox(b,d,f)%positions)
        print *, singleton[1]%halo_outbox(b,d,f)%positions, singleton[1]%halo_outbox(b,d,f)%flux_values
      end do; end do; end do
   end associate
  end procedure

  module procedure set_halo_outbox
    integer b,d,f

    allocate( singleton%halo_outbox(size(my_halo_outbox,1), size(my_halo_outbox,2), size(my_halo_outbox,3)) )

    do b=1,size(my_halo_outbox,1)
      do d=1,size(my_halo_outbox,2)
        do f=1,size(my_halo_outbox,3)
          singleton%halo_outbox(b,d,f)%flux_values = my_halo_outbox(b,d,f)%flux_values
          singleton%halo_outbox(b,d,f)%positions = my_halo_outbox(b,d,f)%positions
        end do
      end do
    end do

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
  type(package), allocatable :: bare(:,:,:)
  integer i, j, k

  allocate( bare(2,1,1) )
  do i=1, size(bare,1)
    do j=1, size(bare,2)
      do k=1, size(bare,3)
        bare(i,j,k)%id = i
        bare(i,j,k)%positions =  [5,4,3,2]
        bare(i,j,k)%flux_values =  [6,6,6]
      end do
    end do
  end do

  call global_grid%block_surfaces%set_halo_outbox(bare)
  call global_grid%block_surfaces%get_surface_normal_spacing
end program main
