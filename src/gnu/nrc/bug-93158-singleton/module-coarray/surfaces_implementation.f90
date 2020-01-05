submodule(surfaces_interface) surfaces_implementation
contains
  module procedure set_halo_data
    allocate(package::singleton%halo_data)
  end procedure
end submodule
