submodule(shot_interface) shot_implementation
  implicit none
contains
  module procedure from_the_field
#ifndef FORD
       this%parabola = parabola()
#endif
  end procedure
end submodule
