submodule(shot_interface) shot_implementation
  implicit none
contains
  module procedure from_the_field
     use parabola_interface, only : parabola
     use physical_constants ,only : g
#ifndef FORD
       this%parabola = parabola()
#endif
  end procedure
end submodule
