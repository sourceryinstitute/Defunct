submodule(shot_interface) shot_implementation
  implicit none
contains
  module procedure from_the_field
     use parabola_interface, only : parabola
     use physical_constants ,only : g
     associate(y0=>release_height,u => speed*cos(inclination),v => speed*sin(inclination))
#ifndef FORD
       this%parabola = parabola( a=-g/(2*u**2), b=v/u ,c=y0)
#endif
     end associate
  end procedure
end submodule
