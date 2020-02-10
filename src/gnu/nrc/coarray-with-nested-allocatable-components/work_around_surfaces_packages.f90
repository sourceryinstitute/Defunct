module assertion_interface
  implicit none
contains
  subroutine assert(assertion, description)
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (.not. assertion) error stop description
  end subroutine
end module

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
    type(package), allocatable :: halo_outbox(:,:)
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
  use assertion_interface, only : assert
  implicit none
  type(surfaces), save :: singleton[*]
contains

  module procedure get_surface_normal_spacing
    integer i, b, d

    associate( me => this_image(), ni => num_images() )

      do b=1,size(singleton%halo_outbox,1); do d=1,size(singleton%halo_outbox,2)

        call assert( &
          allocated(singleton%halo_outbox(b,d)%flux_values) .and. size(singleton%halo_outbox(b,d)%flux_values)==3, &
          "allocated(flux_values) .and. size(flux_values)==3" )

        call assert( &
          allocated(singleton%halo_outbox(b,d)%positions) .and. size(singleton%halo_outbox(b,d)%positions)==4, &
          "allocated(positions) .and. size(positions)==4" )

        call assert( &
          all([singleton%halo_outbox(b,d)%positions==me*[5,4,3,2], singleton%halo_outbox(b,d)%flux_values==me*[6,6,6]]), &
          "positions==me*[5,4,3,2] .and. flux_values==me*[6,6,6]" )

      end do; end do

      do i=1,ni
      do b=1,size(singleton[i]%halo_outbox,1); do d=1,size(singleton[i]%halo_outbox,2)

        call assert( &
          allocated(singleton[i]%halo_outbox(b,d)%flux_values) .and. size(singleton[i]%halo_outbox(b,d)%flux_values)==3, &
          "allocated(flux_values) .and. size(flux_values)==3" )

        call assert( &
          allocated(singleton[i]%halo_outbox(b,d)%positions) .and. size(singleton[i]%halo_outbox(b,d)%positions)==4, &
          "allocated(positions) .and. size(positions)==4" )

        call assert( &
        all([singleton[i]%halo_outbox(b,d)%positions==i*[5,4,3,2], singleton[i]%halo_outbox(b,d)%flux_values==i*[6,6,6]]), &
          "positions==i*[5,4,3,2] .and. flux_values==i*[6,6,6]" )

      end do; end do
      end do
   end associate
  end procedure

  module procedure set_halo_outbox
    integer b,d

    allocate( singleton%halo_outbox( 1, 1) )

   !print *,size(my_halo_outbox)

   ! do b=1,size(singleton%halo_outbox,1)
   !   do d=1,size(singleton%halo_outbox,2)
   !     singleton%halo_outbox(b,d)%id = 0
   !     singleton%halo_outbox(b,d)%flux_values = this_image()*[6,6,6] ! my_halo_outbox(b,d)%flux_values
   !     singleton%halo_outbox(b,d)%positions = this_image()*[5,4,3,2] ! my_halo_outbox(b,d)%positions
   !   end do
   ! end do

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

  associate( me=>this_image() )
    allocate( bare(9,1,1) )
    do i=1, size(bare,1)
      do j=1, size(bare,2)
        do k=1, size(bare,3)
          bare(i,j,k)%id = i
          bare(i,j,k)%positions =  me*[5,4,3,2]
          bare(i,j,k)%flux_values =  me*[6,6,6]
        end do
      end do
    end do
  end associate

  call global_grid%block_surfaces%set_halo_outbox(bare)
  !call global_grid%block_surfaces%get_surface_normal_spacing

  sync all
  if (this_image()==1) print *,"Test passed"
end program main
