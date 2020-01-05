module surfaces_interface
  type package
  end type

  type surfaces
    class(package), allocatable :: halo_data
  end type

  type(surfaces) singleton[*]

  interface
    module subroutine set_halo_data()
    end subroutine
  end interface
end module
