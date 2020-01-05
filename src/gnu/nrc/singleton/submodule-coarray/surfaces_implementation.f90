submodule(surfaces_interface) surfaces_implementation
  type(surfaces) singleton[*]
contains
  module procedure set_halo_data
  end procedure
end submodule
