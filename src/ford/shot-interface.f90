module shot_interface
  use kind_parameters ,only : rkind,ikind
  use parabola_interface, only : parabola
  implicit none
  type ,extends(parabola) :: shot
    real(rkind) :: distance
  contains
    procedure, private :: from_the_field
    generic :: set_trajectory => from_the_field
  end type
  interface
    pure module subroutine from_the_field(this,inclination,speed,release_height,range_)
       implicit none
       class(shot) ,intent(inout) :: this
       real(rkind) ,intent(in) :: inclination
       real(rkind) ,intent(in) :: speed
       real(rkind) ,intent(in) :: release_height
       real(rkind) ,intent(in) :: range_
    end subroutine
   end interface
end module
