submodule(shot_interface) shot_implementation
  ! This module encapsulates a basketball shot in a class that inherits the shape
  ! of the shot trajectory from a parent parabola class.
  use kind_parameters ,only : rkind,ikind ! Import real and integer kind parameters
  use parabola_interface, only : parabola    ! Import parabola class
  implicit none                           ! Prevent implicit typing

contains
  
  ! Initialize a shot: construct its parent parabola from the passed shot angle, speed, and release height
  ! and store the distance from the shot origin to the basket.
  module procedure from_the_field
     use physical_constants ,only : g          ! acceleration due to gravity

     ! Associate useful values with their commonly employed mathematical symbols
     associate(y0=>release_height,u => speed*cos(inclination),v => speed*sin(inclination))
#ifndef FORD
       this%parabola = parabola( a=-g/(2._rkind*u**2), b=v/u ,c=y0)
#endif
     end associate
     this%distance = range_
  end procedure

end submodule
