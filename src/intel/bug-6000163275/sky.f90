module sky_procedures_interface
  implicit none
  interface
    module function local_wind_velocity(positions) result(velocity)
      real positions(:),velocity(size(positions))
    end function
  end interface
end module
submodule(sky_procedures_interface) sky_procedures_implementation
contains
  module function local_wind_velocity(positions) result(velocity)
    real positions(:),velocity(size(positions))
    velocity = 0.
  end function
end submodule
