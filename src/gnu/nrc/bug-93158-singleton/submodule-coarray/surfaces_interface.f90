module surfaces_interface
  type package
  end type

  type surfaces
    class(package), allocatable  :: halo_data
  end type

  interface
    module subroutine set_halo_data()
    end subroutine
  end interface
end module
