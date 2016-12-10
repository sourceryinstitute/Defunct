module shot_interface
  ! This module encapsulates a basketball shot in a class that inherits the shape
  ! of the shot trajectory from a parent parabola class.
  use kind_parameters ,only : rkind,ikind ! Import real and integer kind parameters
  use parabola_interface, only : parabola    ! Import parabola class
  implicit none                           ! Prevent implicit typing

  private        ! Limit the scope of all entities to this module by default
  public :: shot ! Expose the shot class and its public type-bound procedures

  type ,extends(parabola) :: shot ! Shot class (extensible derived type)
    private                       ! Limit the scope of components to this module
    real(rkind) :: distance       ! Distance from shot origin to basket
  contains
    procedure, private :: from_the_field ! Define the shot trajectory
    ! Define one generic name to facilitate the possibility of multiple constructors
    generic :: set_trajectory => from_the_field
  end type

  interface
  
    ! Initialize a shot: construct its parent parabola from the passed shot angle, speed, and release height
    ! and store the distance from the shot origin to the basket.
    pure module subroutine from_the_field(this,inclination,speed,release_height,range_)
       implicit none
       class(shot) ,intent(inout) :: this          ! passed-object dummy argument
       real(rkind) ,intent(in) :: inclination    ! angle the shot makes with the horizontal
       real(rkind) ,intent(in) :: speed          ! initial speed of the ball
       real(rkind) ,intent(in) :: release_height ! initial vertical position of ball
       real(rkind) ,intent(in) :: range_         ! horizontal distance from initial position to basket
    end subroutine

   end interface
  
end module
