  type flux_planes
    integer, allocatable :: normals
  end type

  type package
    type(flux_planes) surface_fluxes(1)
  end type

  type(package) mail[*], halo_data

  mail = halo_data
end
