module surfaces_interface
  implicit none

  type surfaces
    integer, allocatable :: positions(:,:,:, :,:,:, :)
      !! 3 halo_outbox dimensionsi + 4 positions dimensions
      !! if not all dimensions are known at the point of initial allocation, allocate remaining dimensions to sero size
  contains
    procedure, nopass :: define_block_surface_fluxes
    procedure, nopass :: get_halo_positions
    procedure, nopass :: verify_halo_data
  end type

  type(surfaces), save :: singleton[*]

  type conduit
    integer, allocatable :: fluxes(:,:,:, :,:,:)
      !! 3 halo_outbox dimensionsi + 1 surface_normal_fluxes dimenions + 2 fluxes dimensions
      !! if not all dimensions are known at the point of initial allocation, allocate remaining dimensions to sero size
  end type

  type(conduit), save  :: workaround[*]

  interface
    module subroutine define_block_surface_fluxes( source_image )
      implicit none
      integer, intent(in) :: source_image
    end subroutine

    module subroutine get_halo_positions( source_image )
      implicit none
      integer, intent(in) :: source_image
    end subroutine

    module subroutine verify_halo_data( source_image )
      implicit none
      integer, intent(in) :: source_image
    end subroutine
  end interface

end module

submodule(surfaces_interface) surfaces_implementation
  implicit none

  integer, parameter :: initial_positions(2,1,1, 2,1,2, 1) = reshape([1,2,3,4,5,6,7,8], [2,1,1, 2,1,2, 1])
  integer, parameter :: initial_fluxes(2,1,1, 2,1,2) = reshape([1,2,3,4,5,6,7,8], [2,1,1, 2,1,2])

contains

  module procedure define_block_surface_fluxes
    associate(me => this_image())
      if (me==source_image) then
        allocate(singleton%positions, source = initial_positions )
        allocate(workaround%fluxes, source = initial_fluxes)
      end if
    end associate
    sync all
  end procedure

  module procedure get_halo_positions
    integer i, j, k, m, n, p, q

    if (this_image() /= source_image) then

      if (allocated(singleton%positions)) deallocate(singleton%positions)
      allocate(singleton%positions, mold = singleton[source_image]%positions)
      do concurrent( &
        i=1:size(singleton%positions,1), j=1:size(singleton%positions,2), k=1:size(singleton%positions,3), &
        m=1:size(singleton%positions,4), n=1:size(singleton%positions,5), p=1:size(singleton%positions,6), &
        q=1:size(singleton%positions,7) )
        singleton%positions(i,j,k,m,n,p,q) = singleton[source_image]%positions(i,j,k,m,n,p,q)
      end do

      if (allocated(workaround%fluxes)) deallocate(workaround%fluxes)
      allocate(workaround%fluxes, mold = workaround[source_image]%fluxes)
      do concurrent( &
        i=1:size(workaround%fluxes,1), j=1:size(workaround%fluxes,2), k=1:size(workaround%fluxes,3), &
        m=1:size(workaround%fluxes,4), n=1:size(workaround%fluxes,5), p=1:size(workaround%fluxes,6) )
        workaround%fluxes(i,j,k,m,n,p) = workaround[source_image]%fluxes(i,j,k,m,n,p)
      end do

    end if

  end procedure

  module procedure verify_halo_data
    if (any(singleton%positions /= initial_positions)) then
      print*,"incorrect positions on image ",this_image(),":", singleton[source_image]%positions - initial_positions
    else if (any(workaround%fluxes /= initial_fluxes)) then
      print *, "incorrect fluxes on image ",this_image(),":", workaround[source_image]%fluxes - initial_fluxes
    else
      print *, "Test passed on image ",this_image()
    end if
  end procedure

end submodule

program main
  use surfaces_interface, only : surfaces
  implicit none

  type(surfaces) block_surfaces

  integer, parameter :: source_image=1
  if (source_image>num_images()) error stop "insufficient number of images: decrease source_image or launch more images"

  call block_surfaces%define_block_surface_fluxes( source_image )
  call block_surfaces%get_halo_positions( source_image )
  call block_surfaces%verify_halo_data( source_image )

end program
